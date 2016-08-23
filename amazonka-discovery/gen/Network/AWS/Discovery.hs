{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The AWS Application Discovery Service helps Systems Integrators quickly and reliably plan application migration projects by automatically identifying applications running in on-premises data centers, their associated dependencies, and their performance profile.
--
-- Planning data center migrations can involve thousands of workloads that are often deeply interdependent. Application discovery and dependency mapping are important early first steps in the migration process, but difficult to perform at scale due to the lack of automated tools.
--
-- The AWS Application Discovery Service automatically collects configuration and usage data from servers to develop a list of applications, how they perform, and how they are interdependent. This information is securely retained in an AWS Application Discovery Service database which you can export as a CSV file into your preferred visualization tool or cloud migration solution to help reduce the complexity and time in planning your cloud migration.
--
-- The Application Discovery Service is currently available for preview. Only customers who are engaged with <https://aws.amazon.com/professional-services/ AWS Professional Services> or a certified AWS partner can use the service. To see the list of certified partners and request access to the Application Discovery Service, complete the following <http://aws.amazon.com/application-discovery/preview/ preview form>.
--
-- This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for the Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you\'re using. For more information, see <http://aws.amazon.com/tools/#SDKs AWS SDKs>.
--
-- This guide is intended for use with the <http://docs.aws.amazon.com/application-discovery/latest/userguide/what-is-appdiscovery.html AWS Discovery Service User Guide>.
--
-- The following are short descriptions of each API action, organized by function.
--
-- __Managing AWS Agents Using the Application Discovery Service__
--
-- An AWS agent is software that you install on on-premises servers and virtual machines that are targeted for discovery and migration. Agents run on Linux and Windows Server and collect server configuration and activity information about your applications and infrastructure. Specifically, agents collect the following information and send it to the Application Discovery Service using Secure Sockets Layer (SSL) encryption:
--
-- -   User information (user name, home directory)
--
-- -   Group information (name)
--
-- -   List of installed packages
--
-- -   List of kernel modules
--
-- -   All create and stop process events
--
-- -   DNS queries
--
-- -   NIC information
--
-- -   TCP\/UDP process listening ports
--
-- -   TCPV4\/V6 connections
--
-- -   Operating system information
--
-- -   System performance
--
-- -   Process performance
--
-- The Application Discovery Service API includes the following actions to manage AWS agents:
--
-- -   /StartDataCollectionByAgentIds/: Instructs the specified agents to start collecting data. The Application Discovery Service takes several minutes to receive and process data after you initiate data collection.
--
-- -   /StopDataCollectionByAgentIds/: Instructs the specified agents to stop collecting data.
--
-- -   /DescribeAgents/: Lists AWS agents by ID or lists all agents associated with your user account if you did not specify an agent ID. The output includes agent IDs, IP addresses, media access control (MAC) addresses, agent health, host name where the agent resides, and the version number of each agent.
--
-- __Querying Configuration Items__
--
-- A /configuration item/ is an IT asset that was discovered in your data center by an AWS agent. When you use the Application Discovery Service, you can specify filters and query specific configuration items. The service supports Server, Process, and Connection configuration items. This means you can specify a value for the following keys and query your IT assets:
--
-- __Server__
--
-- -   server.HostName
--
-- -   server.osName
--
-- -   server.osVersion
--
-- -   server.configurationId
--
-- -   server.agentId
--
-- __Process__
--
-- -   process.name
--
-- -   process.CommandLine
--
-- -   process.configurationId
--
-- -   server.hostName
--
-- -   server.osName
--
-- -   server.osVersion
--
-- -   server.configurationId
--
-- -   server.agentId
--
-- __Connection__
--
-- -   connection.sourceIp
--
-- -   connection.sourcePort
--
-- -   connection.destinationIp
--
-- -   connection.destinationPort
--
-- -   sourceProcess.configurationId
--
-- -   sourceProcess.commandLine
--
-- -   sourceProcess.name
--
-- -   destinationProcessId.configurationId
--
-- -   destinationProcess.commandLine
--
-- -   destinationProcess.name
--
-- -   sourceServer.configurationId
--
-- -   sourceServer.hostName
--
-- -   sourceServer.osName
--
-- -   sourceServer.osVersion
--
-- -   destinationServer.configurationId
--
-- -   destinationServer.hostName
--
-- -   destinationServer.osName
--
-- -   destinationServer.osVersion
--
-- The Application Discovery Service includes the following actions for querying configuration items.
--
-- -   /DescribeConfigurations/: Retrieves a list of attributes for a specific configuration ID. For example, the output for a /server/ configuration item includes a list of attributes about the server, including host name, operating system, number of network cards, etc.
--
-- -   /ListConfigurations/: Retrieves a list of configuration items according to the criteria you specify in a filter. The filter criteria identify relationship requirements. For example, you can specify filter criteria of process.name with values of /nginx/ and /apache/.
--
-- __Tagging Discovered Configuration Items__
--
-- You can tag discovered configuration items. Tags are metadata that help you categorize IT assets in your data center. Tags use a /key/-/value/ format. For example, '{\"key\": \"serverType\", \"value\": \"webServer\"}'.
--
-- -   /CreateTags/: Creates one or more tags for a configuration items.
--
-- -   /DescribeTags/: Retrieves a list of configuration items that are tagged with a specific tag. /Or/, retrieves a list of all tags assigned to a specific configuration item.
--
-- -   /DeleteTags/: Deletes the association between a configuration item and one or more tags.
--
-- __Exporting Data__
--
-- You can export data as a CSV file to an Amazon S3 bucket or into your preferred visualization tool or cloud migration solution to help reduce the complexity and time in planning your cloud migration.
--
-- -   /ExportConfigurations/: Exports all discovered configuration data to an Amazon S3 bucket. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID which you can query using the GetExportStatus API.
--
-- -   /DescribeExportConfigurations/: Gets the status of the data export. When the export is complete, the service returns an Amazon S3 URL where you can download CSV files that include the data.
--
module Network.AWS.Discovery
    (
    -- * Service Configuration
      discovery

    -- * Errors
    -- $errors

    -- ** AuthorizationErrorException
    , _AuthorizationErrorException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** ServerInternalErrorException
    , _ServerInternalErrorException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeTags
    , module Network.AWS.Discovery.DescribeTags

    -- ** ExportConfigurations
    , module Network.AWS.Discovery.ExportConfigurations

    -- ** StopDataCollectionByAgentIds
    , module Network.AWS.Discovery.StopDataCollectionByAgentIds

    -- ** CreateTags
    , module Network.AWS.Discovery.CreateTags

    -- ** DeleteTags
    , module Network.AWS.Discovery.DeleteTags

    -- ** DescribeConfigurations
    , module Network.AWS.Discovery.DescribeConfigurations

    -- ** ListConfigurations
    , module Network.AWS.Discovery.ListConfigurations

    -- ** DescribeAgents
    , module Network.AWS.Discovery.DescribeAgents

    -- ** DescribeExportConfigurations
    , module Network.AWS.Discovery.DescribeExportConfigurations

    -- ** StartDataCollectionByAgentIds
    , module Network.AWS.Discovery.StartDataCollectionByAgentIds

    -- * Types

    -- ** AgentStatus
    , AgentStatus (..)

    -- ** ConfigurationItemType
    , ConfigurationItemType (..)

    -- ** ExportStatus
    , ExportStatus (..)

    -- ** AgentConfigurationStatus
    , AgentConfigurationStatus
    , agentConfigurationStatus
    , acsAgentId
    , acsOperationSucceeded
    , acsDescription

    -- ** AgentInfo
    , AgentInfo
    , agentInfo
    , aiHostName
    , aiAgentNetworkInfoList
    , aiConnectorId
    , aiHealth
    , aiAgentId
    , aiVersion

    -- ** AgentNetworkInfo
    , AgentNetworkInfo
    , agentNetworkInfo
    , aniIpAddress
    , aniMacAddress

    -- ** ConfigurationTag
    , ConfigurationTag
    , configurationTag
    , ctTimeOfCreation
    , ctConfigurationId
    , ctConfigurationType
    , ctValue
    , ctKey

    -- ** ExportInfo
    , ExportInfo
    , exportInfo
    , eiConfigurationsDownloadURL
    , eiExportId
    , eiExportStatus
    , eiStatusMessage
    , eiExportRequestTime

    -- ** Filter
    , Filter
    , filter'
    , fName
    , fValues
    , fCondition

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfName
    , tfValues
    ) where

import           Network.AWS.Discovery.CreateTags
import           Network.AWS.Discovery.DeleteTags
import           Network.AWS.Discovery.DescribeAgents
import           Network.AWS.Discovery.DescribeConfigurations
import           Network.AWS.Discovery.DescribeExportConfigurations
import           Network.AWS.Discovery.DescribeTags
import           Network.AWS.Discovery.ExportConfigurations
import           Network.AWS.Discovery.ListConfigurations
import           Network.AWS.Discovery.StartDataCollectionByAgentIds
import           Network.AWS.Discovery.StopDataCollectionByAgentIds
import           Network.AWS.Discovery.Types
import           Network.AWS.Discovery.Types.Product
import           Network.AWS.Discovery.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Discovery'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
