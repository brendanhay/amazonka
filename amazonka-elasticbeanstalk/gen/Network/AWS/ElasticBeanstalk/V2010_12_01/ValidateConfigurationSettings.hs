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
    , validateConfigurationSettings
    -- ** Request lenses
    , vcsmApplicationName
    , vcsmOptionSettings
    , vcsmTemplateName
    , vcsmEnvironmentName

    -- * Response
    , ValidateConfigurationSettingsResponse
    -- ** Response lenses
    , csvmMessages
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ValidateConfigurationSettings' request.
validateConfigurationSettings :: Text -- ^ 'vcsmApplicationName'
                              -> [ConfigurationOptionSetting] -- ^ 'vcsmOptionSettings'
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

-- | The name of the application that the configuration template or environment
-- belongs to.
vcsmApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> ValidateConfigurationSettings
    -> f ValidateConfigurationSettings
vcsmApplicationName f x =
    (\y -> x { _vcsmApplicationName = y })
       <$> f (_vcsmApplicationName x)
{-# INLINE vcsmApplicationName #-}

-- | A list of the options and desired values to evaluate.
vcsmOptionSettings
    :: Functor f
    => ([ConfigurationOptionSetting]
    -> f ([ConfigurationOptionSetting]))
    -> ValidateConfigurationSettings
    -> f ValidateConfigurationSettings
vcsmOptionSettings f x =
    (\y -> x { _vcsmOptionSettings = y })
       <$> f (_vcsmOptionSettings x)
{-# INLINE vcsmOptionSettings #-}

-- | The name of the configuration template to validate the settings against.
-- Condition: You cannot specify both this and an environment name.
vcsmTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ValidateConfigurationSettings
    -> f ValidateConfigurationSettings
vcsmTemplateName f x =
    (\y -> x { _vcsmTemplateName = y })
       <$> f (_vcsmTemplateName x)
{-# INLINE vcsmTemplateName #-}

-- | The name of the environment to validate the settings against. Condition:
-- You cannot specify both this and a configuration template name.
vcsmEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ValidateConfigurationSettings
    -> f ValidateConfigurationSettings
vcsmEnvironmentName f x =
    (\y -> x { _vcsmEnvironmentName = y })
       <$> f (_vcsmEnvironmentName x)
{-# INLINE vcsmEnvironmentName #-}

instance ToQuery ValidateConfigurationSettings where
    toQuery = genericQuery def

data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _csvmMessages :: [ValidationMessage]
      -- ^ A list of ValidationMessage.
    } deriving (Show, Generic)

-- | A list of ValidationMessage.
csvmMessages
    :: Functor f
    => ([ValidationMessage]
    -> f ([ValidationMessage]))
    -> ValidateConfigurationSettingsResponse
    -> f ValidateConfigurationSettingsResponse
csvmMessages f x =
    (\y -> x { _csvmMessages = y })
       <$> f (_csvmMessages x)
{-# INLINE csvmMessages #-}

instance FromXML ValidateConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateConfigurationSettings where
    type Sv ValidateConfigurationSettings = ElasticBeanstalk
    type Rs ValidateConfigurationSettings = ValidateConfigurationSettingsResponse

    request = post "ValidateConfigurationSettings"
    response _ = xmlResponse
