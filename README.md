
# ABAP Tool for SOQL Query #

 Author: **Mark Saksornyuth**
## Description ##

ABAP Tool for [SOQL](https://developer.salesforce.com/docs/atlas.en-us.soql_sosl.meta/soql_sosl/sforce_api_calls_soql.htm) Query allows you to easily run SOQL queries in ABAP Platform.

SOQL is similar to the SELECT statement in the widely used Structured Query Language (SQL) but is designed specifically for Salesforce data.

Key Features and Capabilities
 - Simple User interface
 - Support  standard and custom objects
 - Filter and sort query results within ALV grid
 - Export query results

![](/img/main-app.png)


![](/img/query-results.png)

## How do I use it? ##
 - Install and set up abapGit on your SAP System
 - Clone the repository using abapGit

## Setup
SAP Configuration:
 - Import Salesforce certificate in STRUST transaction. 
   Location : SSL Client (Anonymous) 
 - Go to [`https://login.salesforce.com/`](https://login.salesforce.com/) to get the certificates
 - Open the "Certification path" tab, start from the parent node to the root node.

Salesforce Configuration
 - Setup [`a new connected app`](https://help.salesforce.com/articleView?id=000205876&language=en_US&type=1) in Salesforce for OAuth authentication

## Usage ##

Main Program: ZABAP_SOQL_BUILDER

### Setup OAuth Authentication ###
Provide client id, client_secret from Salesforce Configuration step.

```abap
  ls_credentials-grant_type    = 'password'.                
  ls_credentials-client_id     = <client_id>.
  ls_credentials-client_secret = <client_secret>.
  ls_credentials-user_name     = <user_name>.
  ls_credentials-password      = <password>.
``` 
  
## License

MIT-license. See [LICENSE](LICENSE) file.
