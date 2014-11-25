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

-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a set of configuration settings and either a configuration template or
-- environment, and determines whether those values are valid.
--
-- This action returns a list of messages indicating any errors or warnings
-- associated with the selection of option values.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ValidateConfigurationSettings.html>
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    (
    -- * Request
      ValidateConfigurationSettings
    -- ** Request constructor
    , validateConfigurationSettings
    -- ** Request lenses
    , vcsApplicationName
    , vcsEnvironmentName
    , vcsOptionSettings
    , vcsTemplateName

    -- * Response
    , ValidateConfigurationSettingsResponse
    -- ** Response constructor
    , validateConfigurationSettingsResponse
    -- ** Response lenses
    , vcsrMessages
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data ValidateConfigurationSettings = ValidateConfigurationSettings
    { _vcsApplicationName :: Text
    , _vcsEnvironmentName :: Maybe Text
    , _vcsOptionSettings  :: List "OptionSettings" ConfigurationOptionSetting
    , _vcsTemplateName    :: Maybe Text
    } deriving (Eq, Show)

-- | 'ValidateConfigurationSettings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsApplicationName' @::@ 'Text'
--
-- * 'vcsEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'vcsOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'vcsTemplateName' @::@ 'Maybe' 'Text'
--
validateConfigurationSettings :: Text -- ^ 'vcsApplicationName'
                              -> ValidateConfigurationSettings
validateConfigurationSettings p1 = ValidateConfigurationSettings
    { _vcsApplicationName = p1
    , _vcsTemplateName    = Nothing
    , _vcsEnvironmentName = Nothing
    , _vcsOptionSettings  = mempty
    }

-- | The name of the application that the configuration template or environment
-- belongs to.
vcsApplicationName :: Lens' ValidateConfigurationSettings Text
vcsApplicationName =
    lens _vcsApplicationName (\s a -> s { _vcsApplicationName = a })

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template name.
vcsEnvironmentName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsEnvironmentName =
    lens _vcsEnvironmentName (\s a -> s { _vcsEnvironmentName = a })

-- | A list of the options and desired values to evaluate.
vcsOptionSettings :: Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsOptionSettings =
    lens _vcsOptionSettings (\s a -> s { _vcsOptionSettings = a })
        . _List

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
vcsTemplateName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsTemplateName = lens _vcsTemplateName (\s a -> s { _vcsTemplateName = a })

newtype ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _vcsrMessages :: List "Messages" ValidationMessage
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ValidateConfigurationSettingsResponse where
    type Item ValidateConfigurationSettingsResponse = ValidationMessage

    fromList = ValidateConfigurationSettingsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _vcsrMessages

-- | 'ValidateConfigurationSettingsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsrMessages' @::@ ['ValidationMessage']
--
validateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse
validateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _vcsrMessages = mempty
    }

-- | A list of 'ValidationMessage'.
vcsrMessages :: Lens' ValidateConfigurationSettingsResponse [ValidationMessage]
vcsrMessages = lens _vcsrMessages (\s a -> s { _vcsrMessages = a }) . _List

instance ToPath ValidateConfigurationSettings where
    toPath = const "/"

instance ToQuery ValidateConfigurationSettings where
    toQuery ValidateConfigurationSettings{..} = mconcat
        [ "ApplicationName" =? _vcsApplicationName
        , "EnvironmentName" =? _vcsEnvironmentName
        , "OptionSettings"  =? _vcsOptionSettings
        , "TemplateName"    =? _vcsTemplateName
        ]

instance ToHeaders ValidateConfigurationSettings

instance AWSRequest ValidateConfigurationSettings where
    type Sv ValidateConfigurationSettings = ElasticBeanstalk
    type Rs ValidateConfigurationSettings = ValidateConfigurationSettingsResponse

    request  = post "ValidateConfigurationSettings"
    response = xmlResponse

instance FromXML ValidateConfigurationSettingsResponse where
    parseXML = withElement "ValidateConfigurationSettingsResult" $ \x -> ValidateConfigurationSettingsResponse
        <$> x .@  "Messages"
