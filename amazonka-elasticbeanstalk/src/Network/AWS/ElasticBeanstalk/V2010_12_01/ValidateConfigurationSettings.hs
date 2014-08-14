{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ValidateConfigurationSettings' request.
validateConfigurationSettings :: Text -- ^ '_vcsmApplicationName'
                              -> [ConfigurationOptionSetting] -- ^ '_vcsmOptionSettings'
                              -> ValidateConfigurationSettings
validateConfigurationSettings p1 p2 = ValidateConfigurationSettings
    { _vcsmApplicationName = p1
    , _vcsmOptionSettings = p2
    , _vcsmTemplateName = Nothing
    , _vcsmEnvironmentName = Nothing
    }

data ValidateConfigurationSettings = ValidateConfigurationSettings
    { _vcsmApplicationName :: Text
      -- ^ The name of the application that the configuration template or
      -- environment belongs to.
    , _vcsmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the options and desired values to evaluate.
    , _vcsmTemplateName :: Maybe Text
      -- ^ The name of the configuration template to validate the settings
      -- against. Condition: You cannot specify both this and an
      -- environment name.
    , _vcsmEnvironmentName :: Maybe Text
      -- ^ The name of the environment to validate the settings against.
      -- Condition: You cannot specify both this and a configuration
      -- template name.
    } deriving (Show, Generic)

makeLenses ''ValidateConfigurationSettings

instance ToQuery ValidateConfigurationSettings where
    toQuery = genericQuery def

data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _csvmMessages :: [ValidationMessage]
      -- ^ A list of ValidationMessage.
    } deriving (Show, Generic)

makeLenses ''ValidateConfigurationSettingsResponse

instance FromXML ValidateConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateConfigurationSettings where
    type Sv ValidateConfigurationSettings = ElasticBeanstalk
    type Rs ValidateConfigurationSettings = ValidateConfigurationSettingsResponse

    request = post "ValidateConfigurationSettings"
    response _ = xmlResponse
