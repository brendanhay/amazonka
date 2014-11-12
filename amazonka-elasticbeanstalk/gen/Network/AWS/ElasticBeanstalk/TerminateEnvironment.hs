{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified environment.
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Request
      TerminateEnvironmentMessage
    -- ** Request constructor
    , terminateEnvironmentMessage
    -- ** Request lenses
    , temEnvironmentId
    , temEnvironmentName
    , temTerminateResources

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , ed1ApplicationName
    , ed1CNAME
    , ed1DateCreated
    , ed1DateUpdated
    , ed1Description
    , ed1EndpointURL
    , ed1EnvironmentId
    , ed1EnvironmentName
    , ed1Health
    , ed1Resources
    , ed1SolutionStackName
    , ed1Status
    , ed1TemplateName
    , ed1Tier
    , ed1VersionLabel
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data TerminateEnvironmentMessage = TerminateEnvironmentMessage
    { _temEnvironmentId      :: Maybe Text
    , _temEnvironmentName    :: Maybe Text
    , _temTerminateResources :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'TerminateEnvironmentMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'temEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'temEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'temTerminateResources' @::@ 'Maybe' 'Bool'
--
terminateEnvironmentMessage :: TerminateEnvironmentMessage
terminateEnvironmentMessage = TerminateEnvironmentMessage
    { _temEnvironmentId      = Nothing
    , _temEnvironmentName    = Nothing
    , _temTerminateResources = Nothing
    }

-- | The ID of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentName, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentId :: Lens' TerminateEnvironmentMessage (Maybe Text)
temEnvironmentId = lens _temEnvironmentId (\s a -> s { _temEnvironmentId = a })

-- | The name of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentName :: Lens' TerminateEnvironmentMessage (Maybe Text)
temEnvironmentName =
    lens _temEnvironmentName (\s a -> s { _temEnvironmentName = a })

-- | Indicates whether the associated AWS resources should shut down when the
-- environment is terminated: true: (default) The user AWS resources (for
-- example, the Auto Scaling group, LoadBalancer, etc.) are terminated along
-- with the environment. false: The environment is removed from the AWS
-- Elastic Beanstalk but the AWS resources continue to operate. true: The
-- specified environment as well as the associated AWS resources, such as
-- Auto Scaling group and LoadBalancer, are terminated. false: AWS Elastic
-- Beanstalk resource management is removed from the environment, but the
-- AWS resources continue to operate. For more information, see the AWS
-- Elastic Beanstalk User Guide. Default: true Valid Values: true | false.
temTerminateResources :: Lens' TerminateEnvironmentMessage (Maybe Bool)
temTerminateResources =
    lens _temTerminateResources (\s a -> s { _temTerminateResources = a })

instance ToQuery TerminateEnvironmentMessage

instance ToPath TerminateEnvironmentMessage where
    toPath = const "/"

instance AWSRequest TerminateEnvironmentMessage where
    type Sv TerminateEnvironmentMessage = ElasticBeanstalk
    type Rs TerminateEnvironmentMessage = EnvironmentDescription

    request  = post "TerminateEnvironment"
    response = xmlResponse $ const decodeCursor
