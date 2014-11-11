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

-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a set of configuration settings and either a configuration template
-- or environment, and determines whether those values are valid. This action
-- returns a list of messages indicating any errors or warnings associated
-- with the selection of option values.
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    (
    -- * Request
      ValidateConfigurationSettingsMessage
    -- ** Request constructor
    , validateConfigurationSettingsMessage
    -- ** Request lenses
    , vcsmApplicationName
    , vcsmEnvironmentName
    , vcsmOptionSettings
    , vcsmTemplateName

    -- * Response
    , ConfigurationSettingsValidationMessages
    -- ** Response constructor
    , configurationSettingsValidationMessages
    -- ** Response lenses
    , csvmMessages
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data ValidateConfigurationSettingsMessage = ValidateConfigurationSettingsMessage
    { _vcsmApplicationName :: Text
    , _vcsmEnvironmentName :: Maybe Text
    , _vcsmOptionSettings  :: [ConfigurationOptionSetting]
    , _vcsmTemplateName    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ValidateConfigurationSettingsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsmApplicationName' @::@ 'Text'
--
-- * 'vcsmEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'vcsmOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'vcsmTemplateName' @::@ 'Maybe' 'Text'
--
validateConfigurationSettingsMessage :: Text -- ^ 'vcsmApplicationName'
                                     -> ValidateConfigurationSettingsMessage
validateConfigurationSettingsMessage p1 = ValidateConfigurationSettingsMessage
    { _vcsmApplicationName = p1
    , _vcsmTemplateName    = Nothing
    , _vcsmEnvironmentName = Nothing
    , _vcsmOptionSettings  = mempty
    }

-- | The name of the application that the configuration template or
-- environment belongs to.
vcsmApplicationName :: Lens' ValidateConfigurationSettingsMessage Text
vcsmApplicationName =
    lens _vcsmApplicationName (\s a -> s { _vcsmApplicationName = a })

-- | The name of the environment to validate the settings against. Condition:
-- You cannot specify both this and a configuration template name.
vcsmEnvironmentName :: Lens' ValidateConfigurationSettingsMessage (Maybe Text)
vcsmEnvironmentName =
    lens _vcsmEnvironmentName (\s a -> s { _vcsmEnvironmentName = a })

-- | A list of the options and desired values to evaluate.
vcsmOptionSettings :: Lens' ValidateConfigurationSettingsMessage [ConfigurationOptionSetting]
vcsmOptionSettings =
    lens _vcsmOptionSettings (\s a -> s { _vcsmOptionSettings = a })

-- | The name of the configuration template to validate the settings against.
-- Condition: You cannot specify both this and an environment name.
vcsmTemplateName :: Lens' ValidateConfigurationSettingsMessage (Maybe Text)
vcsmTemplateName = lens _vcsmTemplateName (\s a -> s { _vcsmTemplateName = a })
instance ToQuery ValidateConfigurationSettingsMessage

instance ToPath ValidateConfigurationSettingsMessage where
    toPath = const "/"

newtype ConfigurationSettingsValidationMessages = ConfigurationSettingsValidationMessages
    { _csvmMessages :: [ValidationMessage]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'ConfigurationSettingsValidationMessages' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csvmMessages' @::@ ['ValidationMessage']
--
configurationSettingsValidationMessages :: ConfigurationSettingsValidationMessages
configurationSettingsValidationMessages = ConfigurationSettingsValidationMessages
    { _csvmMessages = mempty
    }

-- | A list of ValidationMessage.
csvmMessages :: Lens' ConfigurationSettingsValidationMessages [ValidationMessage]
csvmMessages = lens _csvmMessages (\s a -> s { _csvmMessages = a })
instance FromXML ConfigurationSettingsValidationMessages where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationSettingsValidationMessages"

instance AWSRequest ValidateConfigurationSettingsMessage where
    type Sv ValidateConfigurationSettingsMessage = ElasticBeanstalk
    type Rs ValidateConfigurationSettingsMessage = ConfigurationSettingsValidationMessages

    request  = post "ValidateConfigurationSettings"
    response = xmlResponse $ \h x -> ConfigurationSettingsValidationMessages
        <$> x %| "Messages"
