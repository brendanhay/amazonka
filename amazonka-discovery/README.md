# Amazon Application Discovery Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.2`


## Description

The AWS Application Discovery Service helps Systems Integrators quickly and reliably plan application migration projects by automatically identifying applications running in on-premises data centers, their associated dependencies, and their performance profile.

Planning data center migrations can involve thousands of workloads that are often deeply interdependent. Application discovery and dependency mapping are important early first steps in the migration process, but difficult to perform at scale due to the lack of automated tools.

The AWS Application Discovery Service automatically collects configuration and usage data from servers to develop a list of applications, how they perform, and how they are interdependent. This information is securely retained in an AWS Application Discovery Service database which you can export as a CSV file into your preferred visualization tool or cloud migration solution to help reduce the complexity and time in planning your cloud migration.

The Application Discovery Service is currently available for preview. Only customers who are engaged with <https://aws.amazon.com/professional-services/ AWS Professional Services> or a certified AWS partner can use the service. To see the list of certified partners and request access to the Application Discovery Service, complete the following <http://aws.amazon.com/application-discovery/preview/ preview form>.

This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for the Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you\'re using. For more information, see <http://aws.amazon.com/tools/#SDKs AWS SDKs>.

This guide is intended for use with the <http://docs.aws.amazon.com/application-discovery/latest/userguide/what-is-appdiscovery.html AWS Discovery Service User Guide>.

The following are short descriptions of each API action, organized by function.

__Managing AWS Agents Using the Application Discovery Service__

An AWS agent is software that you install on on-premises servers and virtual machines that are targeted for discovery and migration. Agents run on Linux and Windows Server and collect server configuration and activity information about your applications and infrastructure. Specifically, agents collect the following information and send it to the Application Discovery Service using Secure Sockets Layer (SSL) encryption:

-   User information (user name, home directory)

-   Group information (name)

-   List of installed packages

-   List of kernel modules

-   All create and stop process events

-   DNS queries

-   NIC information

-   TCP\/UDP process listening ports

-   TCPV4\/V6 connections

-   Operating system information

-   System performance

-   Process performance

The Application Discovery Service API includes the following actions to manage AWS agents:

-   /StartDataCollectionByAgentIds/: Instructs the specified agents to start collecting data. The Application Discovery Service takes several minutes to receive and process data after you initiate data collection.

-   /StopDataCollectionByAgentIds/: Instructs the specified agents to stop collecting data.

-   /DescribeAgents/: Lists AWS agents by ID or lists all agents associated with your user account if you did not specify an agent ID. The output includes agent IDs, IP addresses, media access control (MAC) addresses, agent health, host name where the agent resides, and the version number of each agent.

__Querying Configuration Items__

A /configuration item/ is an IT asset that was discovered in your data center by an AWS agent. When you use the Application Discovery Service, you can specify filters and query specific configuration items. The service supports Server, Process, and Connection configuration items. This means you can specify a value for the following keys and query your IT assets:

__Server__

-   server.HostName

-   server.osName

-   server.osVersion

-   server.configurationId

-   server.agentId

__Process__

-   process.name

-   process.CommandLine

-   process.configurationId

-   server.hostName

-   server.osName

-   server.osVersion

-   server.configurationId

-   server.agentId

__Connection__

-   connection.sourceIp

-   connection.sourcePort

-   connection.destinationIp

-   connection.destinationPort

-   sourceProcess.configurationId

-   sourceProcess.commandLine

-   sourceProcess.name

-   destinationProcessId.configurationId

-   destinationProcess.commandLine

-   destinationProcess.name

-   sourceServer.configurationId

-   sourceServer.hostName

-   sourceServer.osName

-   sourceServer.osVersion

-   destinationServer.configurationId

-   destinationServer.hostName

-   destinationServer.osName

-   destinationServer.osVersion

The Application Discovery Service includes the following actions for querying configuration items.

-   /DescribeConfigurations/: Retrieves a list of attributes for a specific configuration ID. For example, the output for a /server/ configuration item includes a list of attributes about the server, including host name, operating system, number of network cards, etc.

-   /ListConfigurations/: Retrieves a list of configuration items according to the criteria you specify in a filter. The filter criteria identify relationship requirements. For example, you can specify filter criteria of process.name with values of /nginx/ and /apache/.

__Tagging Discovered Configuration Items__

You can tag discovered configuration items. Tags are metadata that help you categorize IT assets in your data center. Tags use a /key/-/value/ format. For example, 
    @
    {\"key\": \"serverType\", \"value\": \"webServer\"}
    @
    .

-   /CreateTags/: Creates one or more tags for a configuration items.

-   /DescribeTags/: Retrieves a list of configuration items that are tagged with a specific tag. /Or/, retrieves a list of all tags assigned to a specific configuration item.

-   /DeleteTags/: Deletes the association between a configuration item and one or more tags.

__Exporting Data__

You can export data as a CSV file to an Amazon S3 bucket or into your preferred visualization tool or cloud migration solution to help reduce the complexity and time in planning your cloud migration.

-   /ExportConfigurations/: Exports all discovered configuration data to an Amazon S3 bucket. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID which you can query using the GetExportStatus API.

-   /DescribeExportConfigurations/: Gets the status of the data export. When the export is complete, the service returns an Amazon S3 URL where you can download CSV files that include the data.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-discovery)
and the [AWS API Reference](https://aws.amazon.com/documentation/).

The types from this library are intended to be used with [amazonka](http://hackage.haskell.org/package/amazonka),
which provides mechanisms for specifying AuthN/AuthZ information and sending requests.

Use of lenses is required for constructing and manipulating types.
This is due to the amount of nesting of AWS types and transparency regarding
de/serialisation into more palatable Haskell values.
The provided lenses should be compatible with any of the major lens libraries
[lens](http://hackage.haskell.org/package/lens) or [lens-family-core](http://hackage.haskell.org/package/lens-family-core).

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-discovery` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
