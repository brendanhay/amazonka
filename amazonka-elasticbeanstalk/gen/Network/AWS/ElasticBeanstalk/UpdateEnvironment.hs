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

-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the environment description, deploys a new application version,
-- updates the configuration settings to an entirely new configuration
-- template, or updates select configuration option values in the running
-- environment. Attempting to update both the release and configuration is not
-- allowed and AWS Elastic Beanstalk returns an InvalidParameterCombination
-- error. When updating the configuration settings to a new template or
-- individual settings, a draft configuration is created and
-- DescribeConfigurationSettings for this environment returns two setting
-- descriptions with different DeploymentStatus values.
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    (
    -- * Request
      UpdateEnvironmentMessage
    -- ** Request constructor
    , updateEnvironment
    -- ** Request lenses
    , uemDescription
    , uemEnvironmentId
    , uemEnvironmentName
    , uemOptionSettings
    , uemOptionsToRemove
    , uemTemplateName
    , uemTier
    , uemVersionLabel

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , updateEnvironmentResponse
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

data UpdateEnvironmentMessage = UpdateEnvironmentMessage
    { _uemDescription     :: Maybe Text
    , _uemEnvironmentId   :: Maybe Text
    , _uemEnvironmentName :: Maybe Text
    , _uemOptionSettings  :: [ConfigurationOptionSetting]
    , _uemOptionsToRemove :: [OptionSpecification]
    , _uemTemplateName    :: Maybe Text
    , _uemTier            :: Maybe EnvironmentTier
    , _uemVersionLabel    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateEnvironmentMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uemDescription' @::@ 'Maybe' 'Text'
--
-- * 'uemEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'uemEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'uemOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'uemOptionsToRemove' @::@ ['OptionSpecification']
--
-- * 'uemTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'uemTier' @::@ 'Maybe' 'EnvironmentTier'
--
-- * 'uemVersionLabel' @::@ 'Maybe' 'Text'
--
updateEnvironment :: UpdateEnvironmentMessage
updateEnvironment = UpdateEnvironmentMessage
    { _uemEnvironmentId   = Nothing
    , _uemEnvironmentName = Nothing
    , _uemDescription     = Nothing
    , _uemTier            = Nothing
    , _uemVersionLabel    = Nothing
    , _uemTemplateName    = Nothing
    , _uemOptionSettings  = mempty
    , _uemOptionsToRemove = mempty
    }

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
uemDescription :: Lens' UpdateEnvironmentMessage (Maybe Text)
uemDescription = lens _uemDescription (\s a -> s { _uemDescription = a })

-- | The ID of the environment to update. If no environment with this ID
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
uemEnvironmentId :: Lens' UpdateEnvironmentMessage (Maybe Text)
uemEnvironmentId = lens _uemEnvironmentId (\s a -> s { _uemEnvironmentId = a })

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
uemEnvironmentName :: Lens' UpdateEnvironmentMessage (Maybe Text)
uemEnvironmentName =
    lens _uemEnvironmentName (\s a -> s { _uemEnvironmentName = a })

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
uemOptionSettings :: Lens' UpdateEnvironmentMessage [ConfigurationOptionSetting]
uemOptionSettings =
    lens _uemOptionSettings (\s a -> s { _uemOptionSettings = a })

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
uemOptionsToRemove :: Lens' UpdateEnvironmentMessage [OptionSpecification]
uemOptionsToRemove =
    lens _uemOptionsToRemove (\s a -> s { _uemOptionsToRemove = a })

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
uemTemplateName :: Lens' UpdateEnvironmentMessage (Maybe Text)
uemTemplateName = lens _uemTemplateName (\s a -> s { _uemTemplateName = a })

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the
-- name of the type, AWS Elastic Beanstalk returns InvalidParameterValue
-- error.
uemTier :: Lens' UpdateEnvironmentMessage (Maybe EnvironmentTier)
uemTier = lens _uemTier (\s a -> s { _uemTier = a })

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version is
-- found, returns an InvalidParameterValue error.
uemVersionLabel :: Lens' UpdateEnvironmentMessage (Maybe Text)
uemVersionLabel = lens _uemVersionLabel (\s a -> s { _uemVersionLabel = a })

instance ToPath UpdateEnvironmentMessage where
    toPath = const "/"

instance ToQuery UpdateEnvironmentMessage

instance AWSRequest UpdateEnvironmentMessage where
    type Sv UpdateEnvironmentMessage = ElasticBeanstalk
    type Rs UpdateEnvironmentMessage = EnvironmentDescription

    request  = post "UpdateEnvironment"
    response = xmlResponse $ const decodeCursor
