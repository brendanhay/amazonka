{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- application and are used to deploy different versions of the application with
-- the same configuration settings.
--
-- Related Topics
--
-- 'DescribeConfigurationOptions'   'DescribeConfigurationSettings'   'ListAvailableSolutionStacks'
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateConfigurationTemplate.html>
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    (
    -- * Request
      CreateConfigurationTemplate
    -- ** Request constructor
    , createConfigurationTemplate
    -- ** Request lenses
    , cctApplicationName
    , cctDescription
    , cctEnvironmentId
    , cctOptionSettings
    , cctSolutionStackName
    , cctSourceConfiguration
    , cctTemplateName

    -- * Response
    , CreateConfigurationTemplateResponse
    -- ** Response constructor
    , createConfigurationTemplateResponse
    -- ** Response lenses
    , cctrApplicationName
    , cctrDateCreated
    , cctrDateUpdated
    , cctrDeploymentStatus
    , cctrDescription
    , cctrEnvironmentName
    , cctrOptionSettings
    , cctrSolutionStackName
    , cctrTemplateName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data CreateConfigurationTemplate = CreateConfigurationTemplate
    { _cctApplicationName     :: Text
    , _cctDescription         :: Maybe Text
    , _cctEnvironmentId       :: Maybe Text
    , _cctOptionSettings      :: List "OptionSettings" ConfigurationOptionSetting
    , _cctSolutionStackName   :: Maybe Text
    , _cctSourceConfiguration :: Maybe SourceConfiguration
    , _cctTemplateName        :: Text
    } deriving (Eq, Show)

-- | 'CreateConfigurationTemplate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cctApplicationName' @::@ 'Text'
--
-- * 'cctDescription' @::@ 'Maybe' 'Text'
--
-- * 'cctEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'cctOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'cctSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'cctSourceConfiguration' @::@ 'Maybe' 'SourceConfiguration'
--
-- * 'cctTemplateName' @::@ 'Text'
--
createConfigurationTemplate :: Text -- ^ 'cctApplicationName'
                            -> Text -- ^ 'cctTemplateName'
                            -> CreateConfigurationTemplate
createConfigurationTemplate p1 p2 = CreateConfigurationTemplate
    { _cctApplicationName     = p1
    , _cctTemplateName        = p2
    , _cctSolutionStackName   = Nothing
    , _cctSourceConfiguration = Nothing
    , _cctEnvironmentId       = Nothing
    , _cctDescription         = Nothing
    , _cctOptionSettings      = mempty
    }

-- | The name of the application to associate with this configuration template.
-- If no application is found with this name, AWS Elastic Beanstalk returns an 'InvalidParameterValue' error.
cctApplicationName :: Lens' CreateConfigurationTemplate Text
cctApplicationName =
    lens _cctApplicationName (\s a -> s { _cctApplicationName = a })

-- | Describes this configuration.
cctDescription :: Lens' CreateConfigurationTemplate (Maybe Text)
cctDescription = lens _cctDescription (\s a -> s { _cctDescription = a })

-- | The ID of the environment used with this configuration template.
cctEnvironmentId :: Lens' CreateConfigurationTemplate (Maybe Text)
cctEnvironmentId = lens _cctEnvironmentId (\s a -> s { _cctEnvironmentId = a })

-- | If specified, AWS Elastic Beanstalk sets the specified configuration option
-- to the requested value. The new value overrides the value obtained from the
-- solution stack or the source configuration template.
cctOptionSettings :: Lens' CreateConfigurationTemplate [ConfigurationOptionSetting]
cctOptionSettings =
    lens _cctOptionSettings (\s a -> s { _cctOptionSettings = a })
        . _List

-- | The name of the solution stack used by this configuration. The solution stack
-- specifies the operating system, architecture, and application server for a
-- configuration template. It determines the set of configuration options as
-- well as the possible and default values.
--
-- Use 'ListAvailableSolutionStacks' to obtain a list of available solution
-- stacks.
--
-- A solution stack name or a source configuration parameter must be
-- specified, otherwise AWS Elastic Beanstalk returns an 'InvalidParameterValue'
-- error.
--
-- If a solution stack name is not specified and the source configuration
-- parameter is specified, AWS Elastic Beanstalk uses the same solution stack as
-- the source configuration template.
cctSolutionStackName :: Lens' CreateConfigurationTemplate (Maybe Text)
cctSolutionStackName =
    lens _cctSolutionStackName (\s a -> s { _cctSolutionStackName = a })

-- | If specified, AWS Elastic Beanstalk uses the configuration values from the
-- specified configuration template to create a new configuration.
--
-- Values specified in the 'OptionSettings' parameter of this call overrides any
-- values obtained from the 'SourceConfiguration'.
--
-- If no configuration template is found, returns an 'InvalidParameterValue'
-- error.
--
-- Constraint: If both the solution stack name parameter and the source
-- configuration parameters are specified, the solution stack of the source
-- configuration template must match the specified solution stack name or else
-- AWS Elastic Beanstalk returns an 'InvalidParameterCombination' error.
cctSourceConfiguration :: Lens' CreateConfigurationTemplate (Maybe SourceConfiguration)
cctSourceConfiguration =
    lens _cctSourceConfiguration (\s a -> s { _cctSourceConfiguration = a })

-- | The name of the configuration template.
--
-- Constraint: This name must be unique per application.
--
-- Default: If a configuration template already exists with this name, AWS
-- Elastic Beanstalk returns an 'InvalidParameterValue' error.
cctTemplateName :: Lens' CreateConfigurationTemplate Text
cctTemplateName = lens _cctTemplateName (\s a -> s { _cctTemplateName = a })

data CreateConfigurationTemplateResponse = CreateConfigurationTemplateResponse
    { _cctrApplicationName   :: Maybe Text
    , _cctrDateCreated       :: Maybe RFC822
    , _cctrDateUpdated       :: Maybe RFC822
    , _cctrDeploymentStatus  :: Maybe ConfigurationDeploymentStatus
    , _cctrDescription       :: Maybe Text
    , _cctrEnvironmentName   :: Maybe Text
    , _cctrOptionSettings    :: List "OptionSettings" ConfigurationOptionSetting
    , _cctrSolutionStackName :: Maybe Text
    , _cctrTemplateName      :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateConfigurationTemplateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cctrApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'cctrDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'cctrDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'cctrDeploymentStatus' @::@ 'Maybe' 'ConfigurationDeploymentStatus'
--
-- * 'cctrDescription' @::@ 'Maybe' 'Text'
--
-- * 'cctrEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'cctrOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'cctrSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'cctrTemplateName' @::@ 'Maybe' 'Text'
--
createConfigurationTemplateResponse :: CreateConfigurationTemplateResponse
createConfigurationTemplateResponse = CreateConfigurationTemplateResponse
    { _cctrSolutionStackName = Nothing
    , _cctrApplicationName   = Nothing
    , _cctrTemplateName      = Nothing
    , _cctrDescription       = Nothing
    , _cctrEnvironmentName   = Nothing
    , _cctrDeploymentStatus  = Nothing
    , _cctrDateCreated       = Nothing
    , _cctrDateUpdated       = Nothing
    , _cctrOptionSettings    = mempty
    }

-- | The name of the application associated with this configuration set.
cctrApplicationName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
cctrApplicationName =
    lens _cctrApplicationName (\s a -> s { _cctrApplicationName = a })

-- | The date (in UTC time) when this configuration set was created.
cctrDateCreated :: Lens' CreateConfigurationTemplateResponse (Maybe UTCTime)
cctrDateCreated = lens _cctrDateCreated (\s a -> s { _cctrDateCreated = a }) . mapping _Time

-- | The date (in UTC time) when this configuration set was last modified.
cctrDateUpdated :: Lens' CreateConfigurationTemplateResponse (Maybe UTCTime)
cctrDateUpdated = lens _cctrDateUpdated (\s a -> s { _cctrDateUpdated = a }) . mapping _Time

-- | If this configuration set is associated with an environment, the 'DeploymentStatus' parameter indicates the deployment status of this configuration set:
--
-- 'null': This configuration is not associated with a running environment.
--
-- 'pending': This is a draft configuration that is not deployed to the
-- associated environment but is in the process of deploying.
--
-- 'deployed': This is the configuration that is currently deployed to the
-- associated running environment.
--
-- 'failed': This is a draft configuration, that failed to successfully
-- deploy.
--
-- 'null': This configuration is not associated with a running environment.   'pending': This is a draft configuration that is not deployed to the associated
-- environment but is in the process of deploying.   'deployed': This is the
-- configuration that is currently deployed to the associated running
-- environment.   'failed': This is a draft configuration that failed to
-- successfully deploy.
cctrDeploymentStatus :: Lens' CreateConfigurationTemplateResponse (Maybe ConfigurationDeploymentStatus)
cctrDeploymentStatus =
    lens _cctrDeploymentStatus (\s a -> s { _cctrDeploymentStatus = a })

-- | Describes this configuration set.
cctrDescription :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
cctrDescription = lens _cctrDescription (\s a -> s { _cctrDescription = a })

-- | If not 'null', the name of the environment for this configuration set.
cctrEnvironmentName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
cctrEnvironmentName =
    lens _cctrEnvironmentName (\s a -> s { _cctrEnvironmentName = a })

-- | A list of the configuration options and their values in this configuration
-- set.
cctrOptionSettings :: Lens' CreateConfigurationTemplateResponse [ConfigurationOptionSetting]
cctrOptionSettings =
    lens _cctrOptionSettings (\s a -> s { _cctrOptionSettings = a })
        . _List

-- | The name of the solution stack this configuration set uses.
cctrSolutionStackName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
cctrSolutionStackName =
    lens _cctrSolutionStackName (\s a -> s { _cctrSolutionStackName = a })

-- | If not 'null', the name of the configuration template for this configuration
-- set.
cctrTemplateName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
cctrTemplateName = lens _cctrTemplateName (\s a -> s { _cctrTemplateName = a })

instance ToPath CreateConfigurationTemplate where
    toPath = const "/"

instance ToQuery CreateConfigurationTemplate where
    toQuery CreateConfigurationTemplate{..} = mconcat
        [ "ApplicationName"     =? _cctApplicationName
        , "Description"         =? _cctDescription
        , "EnvironmentId"       =? _cctEnvironmentId
        , "OptionSettings"      =? _cctOptionSettings
        , "SolutionStackName"   =? _cctSolutionStackName
        , "SourceConfiguration" =? _cctSourceConfiguration
        , "TemplateName"        =? _cctTemplateName
        ]

instance ToHeaders CreateConfigurationTemplate

instance AWSRequest CreateConfigurationTemplate where
    type Sv CreateConfigurationTemplate = ElasticBeanstalk
    type Rs CreateConfigurationTemplate = CreateConfigurationTemplateResponse

    request  = post "CreateConfigurationTemplate"
    response = xmlResponse

instance FromXML CreateConfigurationTemplateResponse where
    parseXML = withElement "CreateConfigurationTemplateResult" $ \x -> CreateConfigurationTemplateResponse
        <$> x .@? "ApplicationName"
        <*> x .@? "DateCreated"
        <*> x .@? "DateUpdated"
        <*> x .@? "DeploymentStatus"
        <*> x .@? "Description"
        <*> x .@? "EnvironmentName"
        <*> x .@  "OptionSettings"
        <*> x .@? "SolutionStackName"
        <*> x .@? "TemplateName"
