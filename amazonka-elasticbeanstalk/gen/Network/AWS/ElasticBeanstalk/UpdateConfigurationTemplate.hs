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

-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified configuration template to have the specified
-- properties or configuration option values. Related Topics
-- DescribeConfigurationOptions.
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    (
    -- * Request
      UpdateConfigurationTemplateMessage
    -- ** Request constructor
    , updateConfigurationTemplateMessage
    -- ** Request lenses
    , uctmApplicationName
    , uctmDescription
    , uctmOptionSettings
    , uctmOptionsToRemove
    , uctmTemplateName

    -- * Response
    , ConfigurationSettingsDescription
    -- ** Response constructor
    , configurationSettingsDescription
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

data UpdateConfigurationTemplateMessage = UpdateConfigurationTemplateMessage
    { _uctmApplicationName :: Text
    , _uctmDescription     :: Maybe Text
    , _uctmOptionSettings  :: [ConfigurationOptionSetting]
    , _uctmOptionsToRemove :: [OptionSpecification]
    , _uctmTemplateName    :: Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateConfigurationTemplateMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uctmApplicationName' @::@ 'Text'
--
-- * 'uctmDescription' @::@ 'Maybe' 'Text'
--
-- * 'uctmOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'uctmOptionsToRemove' @::@ ['OptionSpecification']
--
-- * 'uctmTemplateName' @::@ 'Text'
--
updateConfigurationTemplateMessage :: Text -- ^ 'uctmApplicationName'
                                   -> Text -- ^ 'uctmTemplateName'
                                   -> UpdateConfigurationTemplateMessage
updateConfigurationTemplateMessage p1 p2 = UpdateConfigurationTemplateMessage
    { _uctmApplicationName = p1
    , _uctmTemplateName    = p2
    , _uctmDescription     = Nothing
    , _uctmOptionSettings  = mempty
    , _uctmOptionsToRemove = mempty
    }

-- | The name of the application associated with the configuration template to
-- update. If no application is found with this name,
-- UpdateConfigurationTemplate returns an InvalidParameterValue error.
uctmApplicationName :: Lens' UpdateConfigurationTemplateMessage Text
uctmApplicationName =
    lens _uctmApplicationName (\s a -> s { _uctmApplicationName = a })

-- | A new description for the configuration.
uctmDescription :: Lens' UpdateConfigurationTemplateMessage (Maybe Text)
uctmDescription = lens _uctmDescription (\s a -> s { _uctmDescription = a })

-- | A list of configuration option settings to update with the new specified
-- option value.
uctmOptionSettings :: Lens' UpdateConfigurationTemplateMessage [ConfigurationOptionSetting]
uctmOptionSettings =
    lens _uctmOptionSettings (\s a -> s { _uctmOptionSettings = a })

-- | A list of configuration options to remove from the configuration set.
-- Constraint: You can remove only UserDefined configuration options.
uctmOptionsToRemove :: Lens' UpdateConfigurationTemplateMessage [OptionSpecification]
uctmOptionsToRemove =
    lens _uctmOptionsToRemove (\s a -> s { _uctmOptionsToRemove = a })

-- | The name of the configuration template to update. If no configuration
-- template is found with this name, UpdateConfigurationTemplate returns an
-- InvalidParameterValue error.
uctmTemplateName :: Lens' UpdateConfigurationTemplateMessage Text
uctmTemplateName = lens _uctmTemplateName (\s a -> s { _uctmTemplateName = a })

instance ToQuery UpdateConfigurationTemplateMessage

instance ToPath UpdateConfigurationTemplateMessage where
    toPath = const "/"

instance AWSRequest UpdateConfigurationTemplateMessage where
    type Sv UpdateConfigurationTemplateMessage = ElasticBeanstalk
    type Rs UpdateConfigurationTemplateMessage = ConfigurationSettingsDescription

    request  = post "UpdateConfigurationTemplate"
    response = xmlResponse $ const decodeCursor
