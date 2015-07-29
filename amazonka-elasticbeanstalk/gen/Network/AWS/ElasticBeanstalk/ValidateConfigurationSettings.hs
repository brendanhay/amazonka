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
    , vcsTemplateName
    , vcsEnvironmentName
    , vcsApplicationName
    , vcsOptionSettings

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
-- * 'vcsTemplateName'
--
-- * 'vcsEnvironmentName'
--
-- * 'vcsApplicationName'
--
-- * 'vcsOptionSettings'
data ValidateConfigurationSettings = ValidateConfigurationSettings'
    { _vcsTemplateName    :: !(Maybe Text)
    , _vcsEnvironmentName :: !(Maybe Text)
    , _vcsApplicationName :: !Text
    , _vcsOptionSettings  :: ![ConfigurationOptionSetting]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidateConfigurationSettings' smart constructor.
validateConfigurationSettings :: Text -> ValidateConfigurationSettings
validateConfigurationSettings pApplicationName_ =
    ValidateConfigurationSettings'
    { _vcsTemplateName = Nothing
    , _vcsEnvironmentName = Nothing
    , _vcsApplicationName = pApplicationName_
    , _vcsOptionSettings = mempty
    }

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
vcsTemplateName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsTemplateName = lens _vcsTemplateName (\ s a -> s{_vcsTemplateName = a});

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template
-- name.
vcsEnvironmentName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsEnvironmentName = lens _vcsEnvironmentName (\ s a -> s{_vcsEnvironmentName = a});

-- | The name of the application that the configuration template or
-- environment belongs to.
vcsApplicationName :: Lens' ValidateConfigurationSettings Text
vcsApplicationName = lens _vcsApplicationName (\ s a -> s{_vcsApplicationName = a});

-- | A list of the options and desired values to evaluate.
vcsOptionSettings :: Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsOptionSettings = lens _vcsOptionSettings (\ s a -> s{_vcsOptionSettings = a}) . _Coerce;

instance AWSRequest ValidateConfigurationSettings
         where
        type Sv ValidateConfigurationSettings =
             ElasticBeanstalk
        type Rs ValidateConfigurationSettings =
             ValidateConfigurationSettingsResponse
        request = postQuery
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
        toPath = const mempty

instance ToQuery ValidateConfigurationSettings where
        toQuery ValidateConfigurationSettings'{..}
          = mconcat
              ["Action" =:
                 ("ValidateConfigurationSettings" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _vcsTemplateName,
               "EnvironmentName" =: _vcsEnvironmentName,
               "ApplicationName" =: _vcsApplicationName,
               "OptionSettings" =:
                 toQueryList "member" _vcsOptionSettings]

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
validateConfigurationSettingsResponse pStatus_ =
    ValidateConfigurationSettingsResponse'
    { _vcsrsMessages = Nothing
    , _vcsrsStatus = pStatus_
    }

-- | A list of ValidationMessage.
vcsrsMessages :: Lens' ValidateConfigurationSettingsResponse [ValidationMessage]
vcsrsMessages = lens _vcsrsMessages (\ s a -> s{_vcsrsMessages = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
vcsrsStatus :: Lens' ValidateConfigurationSettingsResponse Int
vcsrsStatus = lens _vcsrsStatus (\ s a -> s{_vcsrsStatus = a});
