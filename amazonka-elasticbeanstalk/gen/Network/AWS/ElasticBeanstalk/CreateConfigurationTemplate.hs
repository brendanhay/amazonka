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

-- Module      : Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a configuration template. Templates are associated with a specific
-- application and are used to deploy different versions of the application
-- with the same configuration settings. Related Topics
-- DescribeConfigurationOptions DescribeConfigurationSettings
-- ListAvailableSolutionStacks.
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    (
    -- * Request
      CreateConfigurationTemplateMessage
    -- ** Request constructor
    , createConfigurationTemplate
    -- ** Request lenses
    , cctmApplicationName
    , cctmDescription
    , cctmEnvironmentId
    , cctmOptionSettings
    , cctmSolutionStackName
    , cctmSourceConfiguration
    , cctmTemplateName

    -- * Response
    , ConfigurationSettingsDescription
    -- ** Response constructor
    , createConfigurationTemplateResponse
    -- ** Response lenses
    , csdApplicationName
    , csdDateCreated
    , csdDateUpdated
    , csdDeploymentStatus
    , csdDescription
    , csdEnvironmentName
    , csdOptionSettings
    , csdSolutionStackName
    , csdTemplateName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data CreateConfigurationTemplateMessage = CreateConfigurationTemplateMessage
    { _cctmApplicationName     :: Text
    , _cctmDescription         :: Maybe Text
    , _cctmEnvironmentId       :: Maybe Text
    , _cctmOptionSettings      :: [ConfigurationOptionSetting]
    , _cctmSolutionStackName   :: Maybe Text
    , _cctmSourceConfiguration :: Maybe SourceConfiguration
    , _cctmTemplateName        :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateConfigurationTemplateMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cctmApplicationName' @::@ 'Text'
--
-- * 'cctmDescription' @::@ 'Maybe' 'Text'
--
-- * 'cctmEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'cctmOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'cctmSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'cctmSourceConfiguration' @::@ 'Maybe' 'SourceConfiguration'
--
-- * 'cctmTemplateName' @::@ 'Text'
--
createConfigurationTemplate :: Text -- ^ 'cctmApplicationName'
                            -> Text -- ^ 'cctmTemplateName'
                            -> CreateConfigurationTemplateMessage
createConfigurationTemplate p1 p2 = CreateConfigurationTemplateMessage
    { _cctmApplicationName     = p1
    , _cctmTemplateName        = p2
    , _cctmSolutionStackName   = Nothing
    , _cctmSourceConfiguration = Nothing
    , _cctmEnvironmentId       = Nothing
    , _cctmDescription         = Nothing
    , _cctmOptionSettings      = mempty
    }

-- | The name of the application to associate with this configuration
-- template. If no application is found with this name, AWS Elastic
-- Beanstalk returns an InvalidParameterValue error.
cctmApplicationName :: Lens' CreateConfigurationTemplateMessage Text
cctmApplicationName =
    lens _cctmApplicationName (\s a -> s { _cctmApplicationName = a })

-- | Describes this configuration.
cctmDescription :: Lens' CreateConfigurationTemplateMessage (Maybe Text)
cctmDescription = lens _cctmDescription (\s a -> s { _cctmDescription = a })

-- | The ID of the environment used with this configuration template.
cctmEnvironmentId :: Lens' CreateConfigurationTemplateMessage (Maybe Text)
cctmEnvironmentId =
    lens _cctmEnvironmentId (\s a -> s { _cctmEnvironmentId = a })

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- option to the requested value. The new value overrides the value obtained
-- from the solution stack or the source configuration template.
cctmOptionSettings :: Lens' CreateConfigurationTemplateMessage [ConfigurationOptionSetting]
cctmOptionSettings =
    lens _cctmOptionSettings (\s a -> s { _cctmOptionSettings = a })

-- | The name of the solution stack used by this configuration. The solution
-- stack specifies the operating system, architecture, and application
-- server for a configuration template. It determines the set of
-- configuration options as well as the possible and default values. Use
-- ListAvailableSolutionStacks to obtain a list of available solution
-- stacks. A solution stack name or a source configuration parameter must be
-- specified, otherwise AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. If a solution stack name is not specified
-- and the source configuration parameter is specified, AWS Elastic
-- Beanstalk uses the same solution stack as the source configuration
-- template.
cctmSolutionStackName :: Lens' CreateConfigurationTemplateMessage (Maybe Text)
cctmSolutionStackName =
    lens _cctmSolutionStackName (\s a -> s { _cctmSolutionStackName = a })

-- | If specified, AWS Elastic Beanstalk uses the configuration values from
-- the specified configuration template to create a new configuration.
-- Values specified in the OptionSettings parameter of this call overrides
-- any values obtained from the SourceConfiguration. If no configuration
-- template is found, returns an InvalidParameterValue error. Constraint: If
-- both the solution stack name parameter and the source configuration
-- parameters are specified, the solution stack of the source configuration
-- template must match the specified solution stack name or else AWS Elastic
-- Beanstalk returns an InvalidParameterCombination error.
cctmSourceConfiguration :: Lens' CreateConfigurationTemplateMessage (Maybe SourceConfiguration)
cctmSourceConfiguration =
    lens _cctmSourceConfiguration (\s a -> s { _cctmSourceConfiguration = a })

-- | The name of the configuration template. Constraint: This name must be
-- unique per application. Default: If a configuration template already
-- exists with this name, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cctmTemplateName :: Lens' CreateConfigurationTemplateMessage Text
cctmTemplateName = lens _cctmTemplateName (\s a -> s { _cctmTemplateName = a })

instance ToPath CreateConfigurationTemplateMessage where
    toPath = const "/"

instance ToQuery CreateConfigurationTemplateMessage

instance AWSRequest CreateConfigurationTemplateMessage where
    type Sv CreateConfigurationTemplateMessage = ElasticBeanstalk
    type Rs CreateConfigurationTemplateMessage = ConfigurationSettingsDescription

    request  = post "CreateConfigurationTemplate"
    response = xmlResponse $ const decodeCursor
