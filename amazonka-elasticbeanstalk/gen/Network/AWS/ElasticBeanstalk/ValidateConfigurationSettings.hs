{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes a set of configuration settings and either a configuration template or environment, and determines whether those values are valid.
--
--
-- This action returns a list of messages indicating any errors or warnings associated with the selection of option values.
--
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    (
    -- * Creating a Request
      validateConfigurationSettings
    , ValidateConfigurationSettings
    -- * Request Lenses
    , vcsTemplateName
    , vcsEnvironmentName
    , vcsApplicationName
    , vcsOptionSettings

    -- * Destructuring the Response
    , validateConfigurationSettingsResponse
    , ValidateConfigurationSettingsResponse
    -- * Response Lenses
    , vcsrsMessages
    , vcsrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A list of validation messages for a specified configuration template.
--
--
--
-- /See:/ 'validateConfigurationSettings' smart constructor.
data ValidateConfigurationSettings = ValidateConfigurationSettings'
  { _vcsTemplateName    :: !(Maybe Text)
  , _vcsEnvironmentName :: !(Maybe Text)
  , _vcsApplicationName :: !Text
  , _vcsOptionSettings  :: ![ConfigurationOptionSetting]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidateConfigurationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcsTemplateName' - The name of the configuration template to validate the settings against. Condition: You cannot specify both this and an environment name.
--
-- * 'vcsEnvironmentName' - The name of the environment to validate the settings against. Condition: You cannot specify both this and a configuration template name.
--
-- * 'vcsApplicationName' - The name of the application that the configuration template or environment belongs to.
--
-- * 'vcsOptionSettings' - A list of the options and desired values to evaluate.
validateConfigurationSettings
    :: Text -- ^ 'vcsApplicationName'
    -> ValidateConfigurationSettings
validateConfigurationSettings pApplicationName_ =
  ValidateConfigurationSettings'
    { _vcsTemplateName = Nothing
    , _vcsEnvironmentName = Nothing
    , _vcsApplicationName = pApplicationName_
    , _vcsOptionSettings = mempty
    }


-- | The name of the configuration template to validate the settings against. Condition: You cannot specify both this and an environment name.
vcsTemplateName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsTemplateName = lens _vcsTemplateName (\ s a -> s{_vcsTemplateName = a})

-- | The name of the environment to validate the settings against. Condition: You cannot specify both this and a configuration template name.
vcsEnvironmentName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsEnvironmentName = lens _vcsEnvironmentName (\ s a -> s{_vcsEnvironmentName = a})

-- | The name of the application that the configuration template or environment belongs to.
vcsApplicationName :: Lens' ValidateConfigurationSettings Text
vcsApplicationName = lens _vcsApplicationName (\ s a -> s{_vcsApplicationName = a})

-- | A list of the options and desired values to evaluate.
vcsOptionSettings :: Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsOptionSettings = lens _vcsOptionSettings (\ s a -> s{_vcsOptionSettings = a}) . _Coerce

instance AWSRequest ValidateConfigurationSettings
         where
        type Rs ValidateConfigurationSettings =
             ValidateConfigurationSettingsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "ValidateConfigurationSettingsResult"
              (\ s h x ->
                 ValidateConfigurationSettingsResponse' <$>
                   (x .@? "Messages" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ValidateConfigurationSettings where

instance NFData ValidateConfigurationSettings where

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
               "TemplateName" =: _vcsTemplateName,
               "EnvironmentName" =: _vcsEnvironmentName,
               "ApplicationName" =: _vcsApplicationName,
               "OptionSettings" =:
                 toQueryList "member" _vcsOptionSettings]

-- | Provides a list of validation messages.
--
--
--
-- /See:/ 'validateConfigurationSettingsResponse' smart constructor.
data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'
  { _vcsrsMessages       :: !(Maybe [ValidationMessage])
  , _vcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidateConfigurationSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcsrsMessages' - A list of 'ValidationMessage' .
--
-- * 'vcsrsResponseStatus' - -- | The response status code.
validateConfigurationSettingsResponse
    :: Int -- ^ 'vcsrsResponseStatus'
    -> ValidateConfigurationSettingsResponse
validateConfigurationSettingsResponse pResponseStatus_ =
  ValidateConfigurationSettingsResponse'
    {_vcsrsMessages = Nothing, _vcsrsResponseStatus = pResponseStatus_}


-- | A list of 'ValidationMessage' .
vcsrsMessages :: Lens' ValidateConfigurationSettingsResponse [ValidationMessage]
vcsrsMessages = lens _vcsrsMessages (\ s a -> s{_vcsrsMessages = a}) . _Default . _Coerce

-- | -- | The response status code.
vcsrsResponseStatus :: Lens' ValidateConfigurationSettingsResponse Int
vcsrsResponseStatus = lens _vcsrsResponseStatus (\ s a -> s{_vcsrsResponseStatus = a})

instance NFData ValidateConfigurationSettingsResponse
         where
