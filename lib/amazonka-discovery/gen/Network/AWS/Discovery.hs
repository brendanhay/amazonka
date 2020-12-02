{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Application Discovery Service__
--
-- AWS Application Discovery Service helps you plan application migration projects by automatically identifying servers, virtual machines (VMs), software, and software dependencies running in your on-premises data centers. Application Discovery Service also collects application performance data, which can help you assess the outcome of your migration. The data collected by Application Discovery Service is securely retained in an AWS-hosted and managed database in the cloud. You can export the data as a CSV or XML file into your preferred visualization tool or cloud-migration solution to plan your migration. For more information, see <http://aws.amazon.com/application-discovery/faqs/ AWS Application Discovery Service FAQ> .
--
-- Application Discovery Service offers two modes of operation:
--
--     * __Agentless discovery__ mode is recommended for environments that use VMware vCenter Server. This mode doesn't require you to install an agent on each host. Agentless discovery gathers server information regardless of the operating systems, which minimizes the time required for initial on-premises infrastructure assessment. Agentless discovery doesn't collect information about software and software dependencies. It also doesn't work in non-VMware environments.
--
--     * __Agent-based discovery__ mode collects a richer set of data than agentless discovery by using the AWS Application Discovery Agent, which you install on one or more hosts in your data center. The agent captures infrastructure and application information, including an inventory of installed software applications, system and process performance, resource utilization, and network dependencies between workloads. The information collected by agents is secured at rest and in transit to the Application Discovery Service database in the cloud.
--
--
--
-- We recommend that you use agent-based discovery for non-VMware environments and to collect information about software and software dependencies. You can also run agent-based and agentless discovery simultaneously. Use agentless discovery to quickly complete the initial infrastructure assessment and then install agents on select hosts.
--
-- Application Discovery Service integrates with application discovery solutions from AWS Partner Network (APN) partners. Third-party application discovery tools can query Application Discovery Service and write to the Application Discovery Service database using a public API. You can then import the data into either a visualization tool or cloud-migration solution.
--
-- /Important:/ Application Discovery Service doesn't gather sensitive information. All data is handled according to the <http://aws.amazon.com/privacy/ AWS Privacy Policy> . You can operate Application Discovery Service offline to inspect collected data before it is shared with the service.
--
-- Your AWS account must be granted access to Application Discovery Service, a process called /whitelisting/ . This is true for AWS partners and customers alike. To request access, <http://aws.amazon.com/application-discovery/ sign up for Application Discovery Service> .
--
-- This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for Application Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you're using. For more information, see <http://aws.amazon.com/tools/#SDKs AWS SDKs> .
--
-- This guide is intended for use with the <http://docs.aws.amazon.com/application-discovery/latest/userguide/ /AWS Application Discovery Service User Guide/ > .
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

    -- ** StopDataCollectionByAgentIds
    , module Network.AWS.Discovery.StopDataCollectionByAgentIds

    -- ** CreateTags
    , module Network.AWS.Discovery.CreateTags

    -- ** DeleteTags
    , module Network.AWS.Discovery.DeleteTags

    -- ** DeleteApplications
    , module Network.AWS.Discovery.DeleteApplications

    -- ** UpdateApplication
    , module Network.AWS.Discovery.UpdateApplication

    -- ** DescribeConfigurations
    , module Network.AWS.Discovery.DescribeConfigurations

    -- ** CreateApplication
    , module Network.AWS.Discovery.CreateApplication

    -- ** ListConfigurations
    , module Network.AWS.Discovery.ListConfigurations

    -- ** DescribeAgents
    , module Network.AWS.Discovery.DescribeAgents

    -- ** DescribeExportTasks
    , module Network.AWS.Discovery.DescribeExportTasks

    -- ** StartDataCollectionByAgentIds
    , module Network.AWS.Discovery.StartDataCollectionByAgentIds

    -- ** GetDiscoverySummary
    , module Network.AWS.Discovery.GetDiscoverySummary

    -- ** DisassociateConfigurationItemsFromApplication
    , module Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication

    -- ** AssociateConfigurationItemsToApplication
    , module Network.AWS.Discovery.AssociateConfigurationItemsToApplication

    -- ** ListServerNeighbors
    , module Network.AWS.Discovery.ListServerNeighbors

    -- ** StartExportTask
    , module Network.AWS.Discovery.StartExportTask

    -- * Types

    -- ** AgentStatus
    , AgentStatus (..)

    -- ** ConfigurationItemType
    , ConfigurationItemType (..)

    -- ** ExportDataFormat
    , ExportDataFormat (..)

    -- ** ExportStatus
    , ExportStatus (..)

    -- ** OrderString
    , OrderString (..)

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
    , aiLastHealthPingTime
    , aiAgentNetworkInfoList
    , aiConnectorId
    , aiHealth
    , aiAgentId
    , aiVersion
    , aiCollectionStatus
    , aiRegisteredTime
    , aiAgentType

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

    -- ** CustomerAgentInfo
    , CustomerAgentInfo
    , customerAgentInfo
    , caiActiveAgents
    , caiHealthyAgents
    , caiBlackListedAgents
    , caiShutdownAgents
    , caiUnhealthyAgents
    , caiTotalAgents
    , caiUnknownAgents

    -- ** CustomerConnectorInfo
    , CustomerConnectorInfo
    , customerConnectorInfo
    , cciActiveConnectors
    , cciHealthyConnectors
    , cciBlackListedConnectors
    , cciShutdownConnectors
    , cciUnhealthyConnectors
    , cciTotalConnectors
    , cciUnknownConnectors

    -- ** ExportFilter
    , ExportFilter
    , exportFilter
    , efName
    , efValues
    , efCondition

    -- ** ExportInfo
    , ExportInfo
    , exportInfo
    , eiConfigurationsDownloadURL
    , eiRequestedStartTime
    , eiRequestedEndTime
    , eiIsTruncated
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

    -- ** NeighborConnectionDetail
    , NeighborConnectionDetail
    , neighborConnectionDetail
    , ncdTransportProtocol
    , ncdDestinationPort
    , ncdSourceServerId
    , ncdDestinationServerId
    , ncdConnectionsCount

    -- ** OrderByElement
    , OrderByElement
    , orderByElement
    , obeSortOrder
    , obeFieldName

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

import Network.AWS.Discovery.AssociateConfigurationItemsToApplication
import Network.AWS.Discovery.CreateApplication
import Network.AWS.Discovery.CreateTags
import Network.AWS.Discovery.DeleteApplications
import Network.AWS.Discovery.DeleteTags
import Network.AWS.Discovery.DescribeAgents
import Network.AWS.Discovery.DescribeConfigurations
import Network.AWS.Discovery.DescribeExportTasks
import Network.AWS.Discovery.DescribeTags
import Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
import Network.AWS.Discovery.GetDiscoverySummary
import Network.AWS.Discovery.ListConfigurations
import Network.AWS.Discovery.ListServerNeighbors
import Network.AWS.Discovery.StartDataCollectionByAgentIds
import Network.AWS.Discovery.StartExportTask
import Network.AWS.Discovery.StopDataCollectionByAgentIds
import Network.AWS.Discovery.Types
import Network.AWS.Discovery.UpdateApplication
import Network.AWS.Discovery.Waiters

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
