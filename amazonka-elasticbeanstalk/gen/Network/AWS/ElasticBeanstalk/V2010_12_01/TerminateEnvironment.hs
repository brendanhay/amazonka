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
{-# INLINE terminateEnvironment #-}

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
temEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
temEnvironmentId f x =
    f (_temEnvironmentId x)
        <&> \y -> x { _temEnvironmentId = y }
{-# INLINE temEnvironmentId #-}

-- | The name of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
temEnvironmentName f x =
    f (_temEnvironmentName x)
        <&> \y -> x { _temEnvironmentName = y }
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
temTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
temTerminateResources f x =
    f (_temTerminateResources x)
        <&> \y -> x { _temTerminateResources = y }
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
eeenApplicationName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenApplicationName f x =
    f (_eeenApplicationName x)
        <&> \y -> x { _eeenApplicationName = y }
{-# INLINE eeenApplicationName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeenTemplateName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenTemplateName f x =
    f (_eeenTemplateName x)
        <&> \y -> x { _eeenTemplateName = y }
{-# INLINE eeenTemplateName #-}

-- | The creation date for this environment.
eeenDateCreated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
eeenDateCreated f x =
    f (_eeenDateCreated x)
        <&> \y -> x { _eeenDateCreated = y }
{-# INLINE eeenDateCreated #-}

-- | The URL to the CNAME for this environment.
eeenCNAME :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenCNAME f x =
    f (_eeenCNAME x)
        <&> \y -> x { _eeenCNAME = y }
{-# INLINE eeenCNAME #-}

-- | Describes this environment.
eeenDescription :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenDescription f x =
    f (_eeenDescription x)
        <&> \y -> x { _eeenDescription = y }
{-# INLINE eeenDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeenEndpointURL :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenEndpointURL f x =
    f (_eeenEndpointURL x)
        <&> \y -> x { _eeenEndpointURL = y }
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
eeenHealth :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentHealth)
eeenHealth f x =
    f (_eeenHealth x)
        <&> \y -> x { _eeenHealth = y }
{-# INLINE eeenHealth #-}

-- | The ID of this environment.
eeenEnvironmentId :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenEnvironmentId f x =
    f (_eeenEnvironmentId x)
        <&> \y -> x { _eeenEnvironmentId = y }
{-# INLINE eeenEnvironmentId #-}

-- | The name of this environment.
eeenEnvironmentName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenEnvironmentName f x =
    f (_eeenEnvironmentName x)
        <&> \y -> x { _eeenEnvironmentName = y }
{-# INLINE eeenEnvironmentName #-}

-- | The description of the AWS resources used by this environment.
eeenResources :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
eeenResources f x =
    f (_eeenResources x)
        <&> \y -> x { _eeenResources = y }
{-# INLINE eeenResources #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeenStatus :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentStatus)
eeenStatus f x =
    f (_eeenStatus x)
        <&> \y -> x { _eeenStatus = y }
{-# INLINE eeenStatus #-}

-- | Describes the current tier of this environment.
eeenTier :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentTier)
eeenTier f x =
    f (_eeenTier x)
        <&> \y -> x { _eeenTier = y }
{-# INLINE eeenTier #-}

-- | The name of the SolutionStack deployed with this environment.
eeenSolutionStackName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenSolutionStackName f x =
    f (_eeenSolutionStackName x)
        <&> \y -> x { _eeenSolutionStackName = y }
{-# INLINE eeenSolutionStackName #-}

-- | The last modified date for this environment.
eeenDateUpdated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
eeenDateUpdated f x =
    f (_eeenDateUpdated x)
        <&> \y -> x { _eeenDateUpdated = y }
{-# INLINE eeenDateUpdated #-}

-- | The application version deployed in this environment.
eeenVersionLabel :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenVersionLabel f x =
    f (_eeenVersionLabel x)
        <&> \y -> x { _eeenVersionLabel = y }
{-# INLINE eeenVersionLabel #-}

instance FromXML TerminateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateEnvironment where
    type Sv TerminateEnvironment = ElasticBeanstalk
    type Rs TerminateEnvironment = TerminateEnvironmentResponse

    request = post "TerminateEnvironment"
    response _ = xmlResponse
