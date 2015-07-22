{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an app for a specified stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateApp.html>
module Network.AWS.OpsWorks.CreateApp
    (
    -- * Request
      CreateApp
    -- ** Request constructor
    , createApp
    -- ** Request lenses
    , carqSSLConfiguration
    , carqShortname
    , carqEnableSSL
    , carqEnvironment
    , carqDataSources
    , carqAppSource
    , carqAttributes
    , carqDomains
    , carqDescription
    , carqStackId
    , carqName
    , carqType

    -- * Response
    , CreateAppResponse
    -- ** Response constructor
    , createAppResponse
    -- ** Response lenses
    , carsAppId
    , carsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createApp' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carqSSLConfiguration'
--
-- * 'carqShortname'
--
-- * 'carqEnableSSL'
--
-- * 'carqEnvironment'
--
-- * 'carqDataSources'
--
-- * 'carqAppSource'
--
-- * 'carqAttributes'
--
-- * 'carqDomains'
--
-- * 'carqDescription'
--
-- * 'carqStackId'
--
-- * 'carqName'
--
-- * 'carqType'
data CreateApp = CreateApp'
    { _carqSSLConfiguration :: !(Maybe SSLConfiguration)
    , _carqShortname        :: !(Maybe Text)
    , _carqEnableSSL        :: !(Maybe Bool)
    , _carqEnvironment      :: !(Maybe [EnvironmentVariable])
    , _carqDataSources      :: !(Maybe [DataSource])
    , _carqAppSource        :: !(Maybe Source)
    , _carqAttributes       :: !(Maybe (Map AppAttributesKeys Text))
    , _carqDomains          :: !(Maybe [Text])
    , _carqDescription      :: !(Maybe Text)
    , _carqStackId          :: !Text
    , _carqName             :: !Text
    , _carqType             :: !AppType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateApp' smart constructor.
createApp :: Text -> Text -> AppType -> CreateApp
createApp pStackId_ pName_ pType_ =
    CreateApp'
    { _carqSSLConfiguration = Nothing
    , _carqShortname = Nothing
    , _carqEnableSSL = Nothing
    , _carqEnvironment = Nothing
    , _carqDataSources = Nothing
    , _carqAppSource = Nothing
    , _carqAttributes = Nothing
    , _carqDomains = Nothing
    , _carqDescription = Nothing
    , _carqStackId = pStackId_
    , _carqName = pName_
    , _carqType = pType_
    }

-- | An @SslConfiguration@ object with the SSL configuration.
carqSSLConfiguration :: Lens' CreateApp (Maybe SSLConfiguration)
carqSSLConfiguration = lens _carqSSLConfiguration (\ s a -> s{_carqSSLConfiguration = a});

-- | The app\'s short name.
carqShortname :: Lens' CreateApp (Maybe Text)
carqShortname = lens _carqShortname (\ s a -> s{_carqShortname = a});

-- | Whether to enable SSL for the app.
carqEnableSSL :: Lens' CreateApp (Maybe Bool)
carqEnableSSL = lens _carqEnableSSL (\ s a -> s{_carqEnableSSL = a});

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instance. For more
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
carqEnvironment :: Lens' CreateApp [EnvironmentVariable]
carqEnvironment = lens _carqEnvironment (\ s a -> s{_carqEnvironment = a}) . _Default;

-- | The app\'s data source.
carqDataSources :: Lens' CreateApp [DataSource]
carqDataSources = lens _carqDataSources (\ s a -> s{_carqDataSources = a}) . _Default;

-- | A @Source@ object that specifies the app repository.
carqAppSource :: Lens' CreateApp (Maybe Source)
carqAppSource = lens _carqAppSource (\ s a -> s{_carqAppSource = a});

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
carqAttributes :: Lens' CreateApp (HashMap AppAttributesKeys Text)
carqAttributes = lens _carqAttributes (\ s a -> s{_carqAttributes = a}) . _Default . _Map;

-- | The app virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
carqDomains :: Lens' CreateApp [Text]
carqDomains = lens _carqDomains (\ s a -> s{_carqDomains = a}) . _Default;

-- | A description of the app.
carqDescription :: Lens' CreateApp (Maybe Text)
carqDescription = lens _carqDescription (\ s a -> s{_carqDescription = a});

-- | The stack ID.
carqStackId :: Lens' CreateApp Text
carqStackId = lens _carqStackId (\ s a -> s{_carqStackId = a});

-- | The app name.
carqName :: Lens' CreateApp Text
carqName = lens _carqName (\ s a -> s{_carqName = a});

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS
-- OpsWorks deploys an application to those instances that are members of
-- the corresponding layer. If your app isn\'t one of the standard types,
-- or you prefer to implement your own Deploy recipes, specify @other@.
carqType :: Lens' CreateApp AppType
carqType = lens _carqType (\ s a -> s{_carqType = a});

instance AWSRequest CreateApp where
        type Sv CreateApp = OpsWorks
        type Rs CreateApp = CreateAppResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateAppResponse' <$>
                   (x .?> "AppId") <*> (pure (fromEnum s)))

instance ToHeaders CreateApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateApp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApp where
        toJSON CreateApp'{..}
          = object
              ["SslConfiguration" .= _carqSSLConfiguration,
               "Shortname" .= _carqShortname,
               "EnableSsl" .= _carqEnableSSL,
               "Environment" .= _carqEnvironment,
               "DataSources" .= _carqDataSources,
               "AppSource" .= _carqAppSource,
               "Attributes" .= _carqAttributes,
               "Domains" .= _carqDomains,
               "Description" .= _carqDescription,
               "StackId" .= _carqStackId, "Name" .= _carqName,
               "Type" .= _carqType]

instance ToPath CreateApp where
        toPath = const "/"

instance ToQuery CreateApp where
        toQuery = const mempty

-- | Contains the response to a @CreateApp@ request.
--
-- /See:/ 'createAppResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carsAppId'
--
-- * 'carsStatus'
data CreateAppResponse = CreateAppResponse'
    { _carsAppId  :: !(Maybe Text)
    , _carsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAppResponse' smart constructor.
createAppResponse :: Int -> CreateAppResponse
createAppResponse pStatus_ =
    CreateAppResponse'
    { _carsAppId = Nothing
    , _carsStatus = pStatus_
    }

-- | The app ID.
carsAppId :: Lens' CreateAppResponse (Maybe Text)
carsAppId = lens _carsAppId (\ s a -> s{_carsAppId = a});

-- | FIXME: Undocumented member.
carsStatus :: Lens' CreateAppResponse Int
carsStatus = lens _carsStatus (\ s a -> s{_carsStatus = a});
