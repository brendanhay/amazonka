{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS AppSync provides API actions for creating and interacting with data sources using GraphQL from your application.
--
--
module Network.AWS.AppSync
    (
    -- * Service Configuration
      appSync

    -- * Errors
    -- $errors

    -- ** APIKeyValidityOutOfBoundsException
    , _APIKeyValidityOutOfBoundsException

    -- ** APIKeyLimitExceededException
    , _APIKeyLimitExceededException

    -- ** APILimitExceededException
    , _APILimitExceededException

    -- ** NotFoundException
    , _NotFoundException

    -- ** GraphQLSchemaException
    , _GraphQLSchemaException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateDataSource
    , module Network.AWS.AppSync.UpdateDataSource

    -- ** DeleteDataSource
    , module Network.AWS.AppSync.DeleteDataSource

    -- ** CreateType
    , module Network.AWS.AppSync.CreateType

    -- ** GetGraphqlAPI
    , module Network.AWS.AppSync.GetGraphqlAPI

    -- ** CreateGraphqlAPI
    , module Network.AWS.AppSync.CreateGraphqlAPI

    -- ** StartSchemaCreation
    , module Network.AWS.AppSync.StartSchemaCreation

    -- ** DeleteGraphqlAPI
    , module Network.AWS.AppSync.DeleteGraphqlAPI

    -- ** UpdateGraphqlAPI
    , module Network.AWS.AppSync.UpdateGraphqlAPI

    -- ** GetIntrospectionSchema
    , module Network.AWS.AppSync.GetIntrospectionSchema

    -- ** GetDataSource
    , module Network.AWS.AppSync.GetDataSource

    -- ** DeleteAPIKey
    , module Network.AWS.AppSync.DeleteAPIKey

    -- ** UpdateAPIKey
    , module Network.AWS.AppSync.UpdateAPIKey

    -- ** UpdateType
    , module Network.AWS.AppSync.UpdateType

    -- ** DeleteType
    , module Network.AWS.AppSync.DeleteType

    -- ** CreateDataSource
    , module Network.AWS.AppSync.CreateDataSource

    -- ** ListTypes
    , module Network.AWS.AppSync.ListTypes

    -- ** ListDataSources
    , module Network.AWS.AppSync.ListDataSources

    -- ** UpdateResolver
    , module Network.AWS.AppSync.UpdateResolver

    -- ** DeleteResolver
    , module Network.AWS.AppSync.DeleteResolver

    -- ** ListResolvers
    , module Network.AWS.AppSync.ListResolvers

    -- ** CreateResolver
    , module Network.AWS.AppSync.CreateResolver

    -- ** GetSchemaCreationStatus
    , module Network.AWS.AppSync.GetSchemaCreationStatus

    -- ** ListGraphqlAPIs
    , module Network.AWS.AppSync.ListGraphqlAPIs

    -- ** GetResolver
    , module Network.AWS.AppSync.GetResolver

    -- ** CreateAPIKey
    , module Network.AWS.AppSync.CreateAPIKey

    -- ** ListAPIKeys
    , module Network.AWS.AppSync.ListAPIKeys

    -- ** GetType
    , module Network.AWS.AppSync.GetType

    -- * Types

    -- ** AuthenticationType
    , AuthenticationType (..)

    -- ** DataSourceType
    , DataSourceType (..)

    -- ** DefaultAction
    , DefaultAction (..)

    -- ** FieldLogLevel
    , FieldLogLevel (..)

    -- ** OutputType
    , OutputType (..)

    -- ** SchemaStatus
    , SchemaStatus (..)

    -- ** TypeDefinitionFormat
    , TypeDefinitionFormat (..)

    -- ** APIKey
    , APIKey
    , apiKey
    , akExpires
    , akId
    , akDescription

    -- ** DataSource
    , DataSource
    , dataSource
    , dsServiceRoleARN
    , dsDataSourceARN
    , dsDynamodbConfig
    , dsName
    , dsLambdaConfig
    , dsType
    , dsDescription
    , dsElasticsearchConfig

    -- ** DynamodbDataSourceConfig
    , DynamodbDataSourceConfig
    , dynamodbDataSourceConfig
    , ddscUseCallerCredentials
    , ddscTableName
    , ddscAwsRegion

    -- ** ElasticsearchDataSourceConfig
    , ElasticsearchDataSourceConfig
    , elasticsearchDataSourceConfig
    , edscEndpoint
    , edscAwsRegion

    -- ** GraphqlAPI
    , GraphqlAPI
    , graphqlAPI
    , gaArn
    , gaApiId
    , gaUris
    , gaOpenIdConnectConfig
    , gaName
    , gaUserPoolConfig
    , gaAuthenticationType
    , gaLogConfig

    -- ** LambdaDataSourceConfig
    , LambdaDataSourceConfig
    , lambdaDataSourceConfig
    , ldscLambdaFunctionARN

    -- ** LogConfig
    , LogConfig
    , logConfig
    , lcFieldLogLevel
    , lcCloudWatchLogsRoleARN

    -- ** OpenIdConnectConfig
    , OpenIdConnectConfig
    , openIdConnectConfig
    , oiccAuthTTL
    , oiccClientId
    , oiccIatTTL
    , oiccIssuer

    -- ** Resolver
    , Resolver
    , resolver
    , rTypeName
    , rDataSourceName
    , rRequestMappingTemplate
    , rResolverARN
    , rResponseMappingTemplate
    , rFieldName

    -- ** Type
    , Type
    , type'
    , tArn
    , tDefinition
    , tFormat
    , tName
    , tDescription

    -- ** UserPoolConfig
    , UserPoolConfig
    , userPoolConfig
    , upcAppIdClientRegex
    , upcUserPoolId
    , upcAwsRegion
    , upcDefaultAction
    ) where

import Network.AWS.AppSync.CreateAPIKey
import Network.AWS.AppSync.CreateDataSource
import Network.AWS.AppSync.CreateGraphqlAPI
import Network.AWS.AppSync.CreateResolver
import Network.AWS.AppSync.CreateType
import Network.AWS.AppSync.DeleteAPIKey
import Network.AWS.AppSync.DeleteDataSource
import Network.AWS.AppSync.DeleteGraphqlAPI
import Network.AWS.AppSync.DeleteResolver
import Network.AWS.AppSync.DeleteType
import Network.AWS.AppSync.GetDataSource
import Network.AWS.AppSync.GetGraphqlAPI
import Network.AWS.AppSync.GetIntrospectionSchema
import Network.AWS.AppSync.GetResolver
import Network.AWS.AppSync.GetSchemaCreationStatus
import Network.AWS.AppSync.GetType
import Network.AWS.AppSync.ListAPIKeys
import Network.AWS.AppSync.ListDataSources
import Network.AWS.AppSync.ListGraphqlAPIs
import Network.AWS.AppSync.ListResolvers
import Network.AWS.AppSync.ListTypes
import Network.AWS.AppSync.StartSchemaCreation
import Network.AWS.AppSync.Types
import Network.AWS.AppSync.UpdateAPIKey
import Network.AWS.AppSync.UpdateDataSource
import Network.AWS.AppSync.UpdateGraphqlAPI
import Network.AWS.AppSync.UpdateResolver
import Network.AWS.AppSync.UpdateType
import Network.AWS.AppSync.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AppSync'.
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
