{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified app.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Deploy or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateApp.html>
module Network.AWS.OpsWorks.UpdateApp
    (
    -- * Request
      UpdateApp
    -- ** Request constructor
    , updateApp
    -- ** Request lenses
    , uarqSSLConfiguration
    , uarqEnableSSL
    , uarqEnvironment
    , uarqDataSources
    , uarqAppSource
    , uarqName
    , uarqAttributes
    , uarqType
    , uarqDomains
    , uarqDescription
    , uarqAppId

    -- * Response
    , UpdateAppResponse
    -- ** Response constructor
    , updateAppResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateApp' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarqSSLConfiguration'
--
-- * 'uarqEnableSSL'
--
-- * 'uarqEnvironment'
--
-- * 'uarqDataSources'
--
-- * 'uarqAppSource'
--
-- * 'uarqName'
--
-- * 'uarqAttributes'
--
-- * 'uarqType'
--
-- * 'uarqDomains'
--
-- * 'uarqDescription'
--
-- * 'uarqAppId'
data UpdateApp = UpdateApp'
    { _uarqSSLConfiguration :: !(Maybe SSLConfiguration)
    , _uarqEnableSSL        :: !(Maybe Bool)
    , _uarqEnvironment      :: !(Maybe [EnvironmentVariable])
    , _uarqDataSources      :: !(Maybe [DataSource])
    , _uarqAppSource        :: !(Maybe Source)
    , _uarqName             :: !(Maybe Text)
    , _uarqAttributes       :: !(Maybe (Map AppAttributesKeys Text))
    , _uarqType             :: !(Maybe AppType)
    , _uarqDomains          :: !(Maybe [Text])
    , _uarqDescription      :: !(Maybe Text)
    , _uarqAppId            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateApp' smart constructor.
updateApp :: Text -> UpdateApp
updateApp pAppId_ =
    UpdateApp'
    { _uarqSSLConfiguration = Nothing
    , _uarqEnableSSL = Nothing
    , _uarqEnvironment = Nothing
    , _uarqDataSources = Nothing
    , _uarqAppSource = Nothing
    , _uarqName = Nothing
    , _uarqAttributes = Nothing
    , _uarqType = Nothing
    , _uarqDomains = Nothing
    , _uarqDescription = Nothing
    , _uarqAppId = pAppId_
    }

-- | An @SslConfiguration@ object with the SSL configuration.
uarqSSLConfiguration :: Lens' UpdateApp (Maybe SSLConfiguration)
uarqSSLConfiguration = lens _uarqSSLConfiguration (\ s a -> s{_uarqSSLConfiguration = a});

-- | Whether SSL is enabled for the app.
uarqEnableSSL :: Lens' UpdateApp (Maybe Bool)
uarqEnableSSL = lens _uarqEnableSSL (\ s a -> s{_uarqEnableSSL = a});

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances.For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 10
-- KB (10240 Bytes). This limit should accommodate most if not all use
-- cases. Exceeding it will cause an exception with the message,
-- \"Environment: is too large (maximum is 10KB).\"
--
-- This parameter is supported only by Chef 11.10 stacks. If you have
-- specified one or more environment variables, you cannot modify the
-- stack\'s Chef version.
uarqEnvironment :: Lens' UpdateApp [EnvironmentVariable]
uarqEnvironment = lens _uarqEnvironment (\ s a -> s{_uarqEnvironment = a}) . _Default;

-- | The app\'s data sources.
uarqDataSources :: Lens' UpdateApp [DataSource]
uarqDataSources = lens _uarqDataSources (\ s a -> s{_uarqDataSources = a}) . _Default;

-- | A @Source@ object that specifies the app repository.
uarqAppSource :: Lens' UpdateApp (Maybe Source)
uarqAppSource = lens _uarqAppSource (\ s a -> s{_uarqAppSource = a});

-- | The app name.
uarqName :: Lens' UpdateApp (Maybe Text)
uarqName = lens _uarqName (\ s a -> s{_uarqName = a});

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
uarqAttributes :: Lens' UpdateApp (HashMap AppAttributesKeys Text)
uarqAttributes = lens _uarqAttributes (\ s a -> s{_uarqAttributes = a}) . _Default . _Map;

-- | The app type.
uarqType :: Lens' UpdateApp (Maybe AppType)
uarqType = lens _uarqType (\ s a -> s{_uarqType = a});

-- | The app\'s virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
uarqDomains :: Lens' UpdateApp [Text]
uarqDomains = lens _uarqDomains (\ s a -> s{_uarqDomains = a}) . _Default;

-- | A description of the app.
uarqDescription :: Lens' UpdateApp (Maybe Text)
uarqDescription = lens _uarqDescription (\ s a -> s{_uarqDescription = a});

-- | The app ID.
uarqAppId :: Lens' UpdateApp Text
uarqAppId = lens _uarqAppId (\ s a -> s{_uarqAppId = a});

instance AWSRequest UpdateApp where
        type Sv UpdateApp = OpsWorks
        type Rs UpdateApp = UpdateAppResponse
        request = postJSON
        response = receiveNull UpdateAppResponse'

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
              ["SslConfiguration" .= _uarqSSLConfiguration,
               "EnableSsl" .= _uarqEnableSSL,
               "Environment" .= _uarqEnvironment,
               "DataSources" .= _uarqDataSources,
               "AppSource" .= _uarqAppSource, "Name" .= _uarqName,
               "Attributes" .= _uarqAttributes, "Type" .= _uarqType,
               "Domains" .= _uarqDomains,
               "Description" .= _uarqDescription,
               "AppId" .= _uarqAppId]

instance ToPath UpdateApp where
        toPath = const "/"

instance ToQuery UpdateApp where
        toQuery = const mempty

-- | /See:/ 'updateAppResponse' smart constructor.
data UpdateAppResponse =
    UpdateAppResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAppResponse' smart constructor.
updateAppResponse :: UpdateAppResponse
updateAppResponse = UpdateAppResponse'
