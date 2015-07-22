{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Takes a set of configuration settings and either a configuration
-- template or environment, and determines whether those values are valid.
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
    , vcsrqTemplateName
    , vcsrqEnvironmentName
    , vcsrqApplicationName
    , vcsrqOptionSettings

    -- * Response
    , ValidateConfigurationSettingsResponse
    -- ** Response constructor
    , validateConfigurationSettingsResponse
    -- ** Response lenses
    , vcsrsMessages
    , vcsrsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A list of validation messages for a specified configuration template.
--
-- /See:/ 'validateConfigurationSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsrqTemplateName'
--
-- * 'vcsrqEnvironmentName'
--
-- * 'vcsrqApplicationName'
--
-- * 'vcsrqOptionSettings'
data ValidateConfigurationSettings = ValidateConfigurationSettings'
    { _vcsrqTemplateName    :: !(Maybe Text)
    , _vcsrqEnvironmentName :: !(Maybe Text)
    , _vcsrqApplicationName :: !Text
    , _vcsrqOptionSettings  :: ![ConfigurationOptionSetting]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidateConfigurationSettings' smart constructor.
validateConfigurationSettings :: Text -> ValidateConfigurationSettings
validateConfigurationSettings pApplicationName =
    ValidateConfigurationSettings'
    { _vcsrqTemplateName = Nothing
    , _vcsrqEnvironmentName = Nothing
    , _vcsrqApplicationName = pApplicationName
    , _vcsrqOptionSettings = mempty
    }

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
vcsrqTemplateName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsrqTemplateName = lens _vcsrqTemplateName (\ s a -> s{_vcsrqTemplateName = a});

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template
-- name.
vcsrqEnvironmentName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsrqEnvironmentName = lens _vcsrqEnvironmentName (\ s a -> s{_vcsrqEnvironmentName = a});

-- | The name of the application that the configuration template or
-- environment belongs to.
vcsrqApplicationName :: Lens' ValidateConfigurationSettings Text
vcsrqApplicationName = lens _vcsrqApplicationName (\ s a -> s{_vcsrqApplicationName = a});

-- | A list of the options and desired values to evaluate.
vcsrqOptionSettings :: Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsrqOptionSettings = lens _vcsrqOptionSettings (\ s a -> s{_vcsrqOptionSettings = a});

instance AWSRequest ValidateConfigurationSettings
         where
        type Sv ValidateConfigurationSettings =
             ElasticBeanstalk
        type Rs ValidateConfigurationSettings =
             ValidateConfigurationSettingsResponse
        request = post
        response
          = receiveXMLWrapper
              "ValidateConfigurationSettingsResult"
              (\ s h x ->
                 ValidateConfigurationSettingsResponse' <$>
                   (x .@? "Messages" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ValidateConfigurationSettings
         where
        toHeaders = const mempty

instance ToPath ValidateConfigurationSettings where
        toPath = const "/"

instance ToQuery ValidateConfigurationSettings where
        toQuery ValidateConfigurationSettings'{..}
          = mconcat
              ["Action" =:
                 ("ValidateConfigurationSettings" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _vcsrqTemplateName,
               "EnvironmentName" =: _vcsrqEnvironmentName,
               "ApplicationName" =: _vcsrqApplicationName,
               "OptionSettings" =:
                 toQueryList "member" _vcsrqOptionSettings]

-- | Provides a list of validation messages.
--
-- /See:/ 'validateConfigurationSettingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsrsMessages'
--
-- * 'vcsrsStatus'
data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'
    { _vcsrsMessages :: !(Maybe [ValidationMessage])
    , _vcsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidateConfigurationSettingsResponse' smart constructor.
validateConfigurationSettingsResponse :: Int -> ValidateConfigurationSettingsResponse
validateConfigurationSettingsResponse pStatus =
    ValidateConfigurationSettingsResponse'
    { _vcsrsMessages = Nothing
    , _vcsrsStatus = pStatus
    }

-- | A list of ValidationMessage.
vcsrsMessages :: Lens' ValidateConfigurationSettingsResponse [ValidationMessage]
vcsrsMessages = lens _vcsrsMessages (\ s a -> s{_vcsrsMessages = a}) . _Default;

-- | FIXME: Undocumented member.
vcsrsStatus :: Lens' ValidateConfigurationSettingsResponse Int
vcsrsStatus = lens _vcsrsStatus (\ s a -> s{_vcsrsStatus = a});
