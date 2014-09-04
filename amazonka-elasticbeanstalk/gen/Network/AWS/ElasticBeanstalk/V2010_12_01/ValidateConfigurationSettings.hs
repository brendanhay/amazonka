{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings
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
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleAppVersion
-- &OptionSettings.member.1.Namespace=aws%3Aautoscaling%3Atrigger
-- &OptionSettings.member.1.OptionName=LowerThreshold
-- &OptionSettings.member.1.Value=1000000
-- &Operation=ValidateConfigurationSettings &AuthParams
-- 06f1cfff-f28f-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings
    (
    -- * Request
      ValidateConfigurationSettings
    -- ** Request constructor
    , mkValidateConfigurationSettingsMessage
    -- ** Request lenses
    , vcsmApplicationName
    , vcsmTemplateName
    , vcsmEnvironmentName
    , vcsmOptionSettings

    -- * Response
    , ValidateConfigurationSettingsResponse
    -- ** Response lenses
    , csvmMessages
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateConfigurationSettings' request.
mkValidateConfigurationSettingsMessage :: Text -- ^ 'vcsmApplicationName'
                                       -> [ConfigurationOptionSetting] -- ^ 'vcsmOptionSettings'
                                       -> ValidateConfigurationSettings
mkValidateConfigurationSettingsMessage p1 p2 = ValidateConfigurationSettings
    { _vcsmApplicationName = p1
    , _vcsmTemplateName = Nothing
    , _vcsmEnvironmentName = Nothing
    , _vcsmOptionSettings = p4
    }
{-# INLINE mkValidateConfigurationSettingsMessage #-}

data ValidateConfigurationSettings = ValidateConfigurationSettings
    { _vcsmApplicationName :: Text
      -- ^ The name of the application that the configuration template or
      -- environment belongs to.
    , _vcsmTemplateName :: Maybe Text
      -- ^ The name of the configuration template to validate the settings
      -- against. Condition: You cannot specify both this and an
      -- environment name.
    , _vcsmEnvironmentName :: Maybe Text
      -- ^ The name of the environment to validate the settings against.
      -- Condition: You cannot specify both this and a configuration
      -- template name.
    , _vcsmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the options and desired values to evaluate.
    } deriving (Show, Generic)

-- | The name of the application that the configuration template or environment
-- belongs to.
vcsmApplicationName :: Lens' ValidateConfigurationSettings (Text)
vcsmApplicationName = lens _vcsmApplicationName (\s a -> s { _vcsmApplicationName = a })
{-# INLINE vcsmApplicationName #-}

-- | The name of the configuration template to validate the settings against.
-- Condition: You cannot specify both this and an environment name.
vcsmTemplateName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsmTemplateName = lens _vcsmTemplateName (\s a -> s { _vcsmTemplateName = a })
{-# INLINE vcsmTemplateName #-}

-- | The name of the environment to validate the settings against. Condition:
-- You cannot specify both this and a configuration template name.
vcsmEnvironmentName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsmEnvironmentName = lens _vcsmEnvironmentName (\s a -> s { _vcsmEnvironmentName = a })
{-# INLINE vcsmEnvironmentName #-}

-- | A list of the options and desired values to evaluate.
vcsmOptionSettings :: Lens' ValidateConfigurationSettings ([ConfigurationOptionSetting])
vcsmOptionSettings = lens _vcsmOptionSettings (\s a -> s { _vcsmOptionSettings = a })
{-# INLINE vcsmOptionSettings #-}

instance ToQuery ValidateConfigurationSettings where
    toQuery = genericQuery def

newtype ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _csvmMessages :: [ValidationMessage]
      -- ^ A list of ValidationMessage.
    } deriving (Show, Generic)

-- | A list of ValidationMessage.
csvmMessages :: Lens' ValidateConfigurationSettingsResponse ([ValidationMessage])
csvmMessages = lens _csvmMessages (\s a -> s { _csvmMessages = a })
{-# INLINE csvmMessages #-}

instance FromXML ValidateConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateConfigurationSettings where
    type Sv ValidateConfigurationSettings = ElasticBeanstalk
    type Rs ValidateConfigurationSettings = ValidateConfigurationSettingsResponse

    request = post "ValidateConfigurationSettings"
    response _ = xmlResponse
