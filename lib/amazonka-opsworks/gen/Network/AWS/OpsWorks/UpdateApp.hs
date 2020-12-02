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
-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified app.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateApp
    (
    -- * Creating a Request
      updateApp
    , UpdateApp
    -- * Request Lenses
    , uaSSLConfiguration
    , uaEnvironment
    , uaEnableSSL
    , uaDataSources
    , uaAppSource
    , uaAttributes
    , uaName
    , uaType
    , uaDomains
    , uaDescription
    , uaAppId

    -- * Destructuring the Response
    , updateAppResponse
    , UpdateAppResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApp' smart constructor.
data UpdateApp = UpdateApp'
  { _uaSSLConfiguration :: !(Maybe SSLConfiguration)
  , _uaEnvironment      :: !(Maybe [EnvironmentVariable])
  , _uaEnableSSL        :: !(Maybe Bool)
  , _uaDataSources      :: !(Maybe [DataSource])
  , _uaAppSource        :: !(Maybe Source)
  , _uaAttributes       :: !(Maybe (Map AppAttributesKeys Text))
  , _uaName             :: !(Maybe Text)
  , _uaType             :: !(Maybe AppType)
  , _uaDomains          :: !(Maybe [Text])
  , _uaDescription      :: !(Maybe Text)
  , _uaAppId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaSSLConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- * 'uaEnvironment' - An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> . There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 10 KB (10240 Bytes). This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 10KB)."
--
-- * 'uaEnableSSL' - Whether SSL is enabled for the app.
--
-- * 'uaDataSources' - The app's data sources.
--
-- * 'uaAppSource' - A @Source@ object that specifies the app repository.
--
-- * 'uaAttributes' - One or more user-defined key/value pairs to be added to the stack attributes.
--
-- * 'uaName' - The app name.
--
-- * 'uaType' - The app type.
--
-- * 'uaDomains' - The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- * 'uaDescription' - A description of the app.
--
-- * 'uaAppId' - The app ID.
updateApp
    :: Text -- ^ 'uaAppId'
    -> UpdateApp
updateApp pAppId_ =
  UpdateApp'
    { _uaSSLConfiguration = Nothing
    , _uaEnvironment = Nothing
    , _uaEnableSSL = Nothing
    , _uaDataSources = Nothing
    , _uaAppSource = Nothing
    , _uaAttributes = Nothing
    , _uaName = Nothing
    , _uaType = Nothing
    , _uaDomains = Nothing
    , _uaDescription = Nothing
    , _uaAppId = pAppId_
    }


-- | An @SslConfiguration@ object with the SSL configuration.
uaSSLConfiguration :: Lens' UpdateApp (Maybe SSLConfiguration)
uaSSLConfiguration = lens _uaSSLConfiguration (\ s a -> s{_uaSSLConfiguration = a})

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> . There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 10 KB (10240 Bytes). This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 10KB)."
uaEnvironment :: Lens' UpdateApp [EnvironmentVariable]
uaEnvironment = lens _uaEnvironment (\ s a -> s{_uaEnvironment = a}) . _Default . _Coerce

-- | Whether SSL is enabled for the app.
uaEnableSSL :: Lens' UpdateApp (Maybe Bool)
uaEnableSSL = lens _uaEnableSSL (\ s a -> s{_uaEnableSSL = a})

-- | The app's data sources.
uaDataSources :: Lens' UpdateApp [DataSource]
uaDataSources = lens _uaDataSources (\ s a -> s{_uaDataSources = a}) . _Default . _Coerce

-- | A @Source@ object that specifies the app repository.
uaAppSource :: Lens' UpdateApp (Maybe Source)
uaAppSource = lens _uaAppSource (\ s a -> s{_uaAppSource = a})

-- | One or more user-defined key/value pairs to be added to the stack attributes.
uaAttributes :: Lens' UpdateApp (HashMap AppAttributesKeys Text)
uaAttributes = lens _uaAttributes (\ s a -> s{_uaAttributes = a}) . _Default . _Map

-- | The app name.
uaName :: Lens' UpdateApp (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | The app type.
uaType :: Lens' UpdateApp (Maybe AppType)
uaType = lens _uaType (\ s a -> s{_uaType = a})

-- | The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
uaDomains :: Lens' UpdateApp [Text]
uaDomains = lens _uaDomains (\ s a -> s{_uaDomains = a}) . _Default . _Coerce

-- | A description of the app.
uaDescription :: Lens' UpdateApp (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | The app ID.
uaAppId :: Lens' UpdateApp Text
uaAppId = lens _uaAppId (\ s a -> s{_uaAppId = a})

instance AWSRequest UpdateApp where
        type Rs UpdateApp = UpdateAppResponse
        request = postJSON opsWorks
        response = receiveNull UpdateAppResponse'

instance Hashable UpdateApp where

instance NFData UpdateApp where

instance ToHeaders UpdateApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateApp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApp where
        toJSON UpdateApp'{..}
          = object
              (catMaybes
                 [("SslConfiguration" .=) <$> _uaSSLConfiguration,
                  ("Environment" .=) <$> _uaEnvironment,
                  ("EnableSsl" .=) <$> _uaEnableSSL,
                  ("DataSources" .=) <$> _uaDataSources,
                  ("AppSource" .=) <$> _uaAppSource,
                  ("Attributes" .=) <$> _uaAttributes,
                  ("Name" .=) <$> _uaName, ("Type" .=) <$> _uaType,
                  ("Domains" .=) <$> _uaDomains,
                  ("Description" .=) <$> _uaDescription,
                  Just ("AppId" .= _uaAppId)])

instance ToPath UpdateApp where
        toPath = const "/"

instance ToQuery UpdateApp where
        toQuery = const mempty

-- | /See:/ 'updateAppResponse' smart constructor.
data UpdateAppResponse =
  UpdateAppResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAppResponse' with the minimum fields required to make a request.
--
updateAppResponse
    :: UpdateAppResponse
updateAppResponse = UpdateAppResponse'


instance NFData UpdateAppResponse where
