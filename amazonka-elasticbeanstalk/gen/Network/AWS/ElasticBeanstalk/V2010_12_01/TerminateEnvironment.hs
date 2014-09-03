{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-icsgecu3wf
-- &EnvironmentName=SampleApp &TerminateResources=true
-- &Operation=TerminateEnvironment &AuthParams Version1 Terminating SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Grey e-icsgecu3wf
-- 2010-11-17T17:10:41.976Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 9b71af21-f26d-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
    (
    -- * Request
      TerminateEnvironment
    -- ** Request constructor
    , terminateEnvironment
    -- ** Request lenses
    , temEnvironmentId
    , temEnvironmentName
    , temTerminateResources

    -- * Response
    , TerminateEnvironmentResponse
    -- ** Response lenses
    , eeenApplicationName
    , eeenTemplateName
    , eeenDateCreated
    , eeenCNAME
    , eeenDescription
    , eeenEndpointURL
    , eeenHealth
    , eeenEnvironmentId
    , eeenEnvironmentName
    , eeenResources
    , eeenStatus
    , eeenTier
    , eeenSolutionStackName
    , eeenDateUpdated
    , eeenVersionLabel
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'TerminateEnvironment' request.
terminateEnvironment :: TerminateEnvironment
terminateEnvironment = TerminateEnvironment
    { _temEnvironmentId = Nothing
    , _temEnvironmentName = Nothing
    , _temTerminateResources = Nothing
    }

data TerminateEnvironment = TerminateEnvironment
    { _temEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to terminate. Condition: You must
      -- specify either this or an EnvironmentName, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _temEnvironmentName :: Maybe Text
      -- ^ The name of the environment to terminate. Condition: You must
      -- specify either this or an EnvironmentId, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _temTerminateResources :: Maybe Bool
      -- ^ Indicates whether the associated AWS resources should shut down
      -- when the environment is terminated: true: (default) The user AWS
      -- resources (for example, the Auto Scaling group, LoadBalancer,
      -- etc.) are terminated along with the environment. false: The
      -- environment is removed from the AWS Elastic Beanstalk but the AWS
      -- resources continue to operate. true: The specified environment as
      -- well as the associated AWS resources, such as Auto Scaling group
      -- and LoadBalancer, are terminated. false: AWS Elastic Beanstalk
      -- resource management is removed from the environment, but the AWS
      -- resources continue to operate. For more information, see the AWS
      -- Elastic Beanstalk User Guide. Default: true Valid Values: true |
      -- false.
    } deriving (Show, Generic)

-- | The ID of the environment to terminate. Condition: You must specify either
-- this or an EnvironmentName, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironment
    -> f TerminateEnvironment
temEnvironmentId f x =
    (\y -> x { _temEnvironmentId = y })
       <$> f (_temEnvironmentId x)
{-# INLINE temEnvironmentId #-}

-- | The name of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironment
    -> f TerminateEnvironment
temEnvironmentName f x =
    (\y -> x { _temEnvironmentName = y })
       <$> f (_temEnvironmentName x)
{-# INLINE temEnvironmentName #-}

-- | Indicates whether the associated AWS resources should shut down when the
-- environment is terminated: true: (default) The user AWS resources (for
-- example, the Auto Scaling group, LoadBalancer, etc.) are terminated along
-- with the environment. false: The environment is removed from the AWS
-- Elastic Beanstalk but the AWS resources continue to operate. true: The
-- specified environment as well as the associated AWS resources, such as Auto
-- Scaling group and LoadBalancer, are terminated. false: AWS Elastic
-- Beanstalk resource management is removed from the environment, but the AWS
-- resources continue to operate. For more information, see the AWS Elastic
-- Beanstalk User Guide. Default: true Valid Values: true | false.
temTerminateResources
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TerminateEnvironment
    -> f TerminateEnvironment
temTerminateResources f x =
    (\y -> x { _temTerminateResources = y })
       <$> f (_temTerminateResources x)
{-# INLINE temTerminateResources #-}

instance ToQuery TerminateEnvironment where
    toQuery = genericQuery def

data TerminateEnvironmentResponse = TerminateEnvironmentResponse
    { _eeenApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _eeenTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _eeenDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _eeenCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _eeenDescription :: Maybe Text
      -- ^ Describes this environment.
    , _eeenEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _eeenHealth :: Maybe EnvironmentHealth
      -- ^ Describes the health status of the environment. AWS Elastic
      -- Beanstalk indicates the failure levels for a running environment:
      -- Red : Indicates the environment is not working. Yellow: Indicates
      -- that something is wrong, the application might not be available,
      -- but the instances appear running. Green: Indicates the
      -- environment is healthy and fully functional. Red: Indicates the
      -- environment is not responsive. Occurs when three or more
      -- consecutive failures occur for an environment. Yellow: Indicates
      -- that something is wrong. Occurs when two consecutive failures
      -- occur for an environment. Green: Indicates the environment is
      -- healthy and fully functional. Grey: Default health for a new
      -- environment. The environment is not fully launched and health
      -- checks have not started or health checks are suspended during an
      -- UpdateEnvironment or RestartEnvironement request. Default: Grey.
    , _eeenEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _eeenEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _eeenResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _eeenStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _eeenTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    , _eeenSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _eeenDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _eeenVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    } deriving (Show, Generic)

-- | The name of the application associated with this environment.
eeenApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenApplicationName f x =
    (\y -> x { _eeenApplicationName = y })
       <$> f (_eeenApplicationName x)
{-# INLINE eeenApplicationName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeenTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenTemplateName f x =
    (\y -> x { _eeenTemplateName = y })
       <$> f (_eeenTemplateName x)
{-# INLINE eeenTemplateName #-}

-- | The creation date for this environment.
eeenDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenDateCreated f x =
    (\y -> x { _eeenDateCreated = y })
       <$> f (_eeenDateCreated x)
{-# INLINE eeenDateCreated #-}

-- | The URL to the CNAME for this environment.
eeenCNAME
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenCNAME f x =
    (\y -> x { _eeenCNAME = y })
       <$> f (_eeenCNAME x)
{-# INLINE eeenCNAME #-}

-- | Describes this environment.
eeenDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenDescription f x =
    (\y -> x { _eeenDescription = y })
       <$> f (_eeenDescription x)
{-# INLINE eeenDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeenEndpointURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenEndpointURL f x =
    (\y -> x { _eeenEndpointURL = y })
       <$> f (_eeenEndpointURL x)
{-# INLINE eeenEndpointURL #-}

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates the
-- environment is not working. Yellow: Indicates that something is wrong, the
-- application might not be available, but the instances appear running.
-- Green: Indicates the environment is healthy and fully functional. Red:
-- Indicates the environment is not responsive. Occurs when three or more
-- consecutive failures occur for an environment. Yellow: Indicates that
-- something is wrong. Occurs when two consecutive failures occur for an
-- environment. Green: Indicates the environment is healthy and fully
-- functional. Grey: Default health for a new environment. The environment is
-- not fully launched and health checks have not started or health checks are
-- suspended during an UpdateEnvironment or RestartEnvironement request.
-- Default: Grey.
eeenHealth
    :: Functor f
    => (Maybe EnvironmentHealth
    -> f (Maybe EnvironmentHealth))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenHealth f x =
    (\y -> x { _eeenHealth = y })
       <$> f (_eeenHealth x)
{-# INLINE eeenHealth #-}

-- | The ID of this environment.
eeenEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenEnvironmentId f x =
    (\y -> x { _eeenEnvironmentId = y })
       <$> f (_eeenEnvironmentId x)
{-# INLINE eeenEnvironmentId #-}

-- | The name of this environment.
eeenEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenEnvironmentName f x =
    (\y -> x { _eeenEnvironmentName = y })
       <$> f (_eeenEnvironmentName x)
{-# INLINE eeenEnvironmentName #-}

-- | The description of the AWS resources used by this environment.
eeenResources
    :: Functor f
    => (Maybe EnvironmentResourcesDescription
    -> f (Maybe EnvironmentResourcesDescription))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenResources f x =
    (\y -> x { _eeenResources = y })
       <$> f (_eeenResources x)
{-# INLINE eeenResources #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeenStatus
    :: Functor f
    => (Maybe EnvironmentStatus
    -> f (Maybe EnvironmentStatus))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenStatus f x =
    (\y -> x { _eeenStatus = y })
       <$> f (_eeenStatus x)
{-# INLINE eeenStatus #-}

-- | Describes the current tier of this environment.
eeenTier
    :: Functor f
    => (Maybe EnvironmentTier
    -> f (Maybe EnvironmentTier))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenTier f x =
    (\y -> x { _eeenTier = y })
       <$> f (_eeenTier x)
{-# INLINE eeenTier #-}

-- | The name of the SolutionStack deployed with this environment.
eeenSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenSolutionStackName f x =
    (\y -> x { _eeenSolutionStackName = y })
       <$> f (_eeenSolutionStackName x)
{-# INLINE eeenSolutionStackName #-}

-- | The last modified date for this environment.
eeenDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenDateUpdated f x =
    (\y -> x { _eeenDateUpdated = y })
       <$> f (_eeenDateUpdated x)
{-# INLINE eeenDateUpdated #-}

-- | The application version deployed in this environment.
eeenVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TerminateEnvironmentResponse
    -> f TerminateEnvironmentResponse
eeenVersionLabel f x =
    (\y -> x { _eeenVersionLabel = y })
       <$> f (_eeenVersionLabel x)
{-# INLINE eeenVersionLabel #-}

instance FromXML TerminateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateEnvironment where
    type Sv TerminateEnvironment = ElasticBeanstalk
    type Rs TerminateEnvironment = TerminateEnvironmentResponse

    request = post "TerminateEnvironment"
    response _ = xmlResponse
