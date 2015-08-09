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
-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a platform application object for one of the supported push
-- notification services, such as APNS and GCM, to which devices and mobile
-- apps may register. You must specify PlatformPrincipal and
-- PlatformCredential attributes when using the @CreatePlatformApplication@
-- action. The PlatformPrincipal is received from the notification service.
-- For APNS\/APNS_SANDBOX, PlatformPrincipal is \"SSL certificate\". For
-- GCM, PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is
-- \"client id\". The PlatformCredential is also received from the
-- notification service. For APNS\/APNS_SANDBOX, PlatformCredential is
-- \"private key\". For GCM, PlatformCredential is \"API key\". For ADM,
-- PlatformCredential is \"client secret\". The PlatformApplicationArn that
-- is returned when using @CreatePlatformApplication@ is then used as an
-- attribute for the @CreatePlatformEndpoint@ action. For more information,
-- see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_CreatePlatformApplication.html AWS API Reference> for CreatePlatformApplication.
module Network.AWS.SNS.CreatePlatformApplication
    (
    -- * Creating a Request
      CreatePlatformApplication
    , createPlatformApplication
    -- * Request Lenses
    , cpaName
    , cpaPlatform
    , cpaAttributes

    -- * Destructuring the Response
    , CreatePlatformApplicationResponse
    , createPlatformApplicationResponse
    -- * Response Lenses
    , cparsPlatformApplicationARN
    , cparsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for CreatePlatformApplication action.
--
-- /See:/ 'createPlatformApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpaName'
--
-- * 'cpaPlatform'
--
-- * 'cpaAttributes'
data CreatePlatformApplication = CreatePlatformApplication'
    { _cpaName       :: !Text
    , _cpaPlatform   :: !Text
    , _cpaAttributes :: !(Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlatformApplication' smart constructor.
createPlatformApplication :: Text -> Text -> CreatePlatformApplication
createPlatformApplication pName_ pPlatform_ =
    CreatePlatformApplication'
    { _cpaName = pName_
    , _cpaPlatform = pPlatform_
    , _cpaAttributes = mempty
    }

-- | Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between
-- 1 and 256 characters long.
cpaName :: Lens' CreatePlatformApplication Text
cpaName = lens _cpaName (\ s a -> s{_cpaName = a});

-- | The following platforms are supported: ADM (Amazon Device Messaging),
-- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Google
-- Cloud Messaging).
cpaPlatform :: Lens' CreatePlatformApplication Text
cpaPlatform = lens _cpaPlatform (\ s a -> s{_cpaPlatform = a});

-- | For a list of attributes, see
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>
cpaAttributes :: Lens' CreatePlatformApplication (HashMap Text Text)
cpaAttributes = lens _cpaAttributes (\ s a -> s{_cpaAttributes = a}) . _Map;

instance AWSRequest CreatePlatformApplication where
        type Sv CreatePlatformApplication = SNS
        type Rs CreatePlatformApplication =
             CreatePlatformApplicationResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreatePlatformApplicationResult"
              (\ s h x ->
                 CreatePlatformApplicationResponse' <$>
                   (x .@? "PlatformApplicationArn") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreatePlatformApplication where
        toHeaders = const mempty

instance ToPath CreatePlatformApplication where
        toPath = const "/"

instance ToQuery CreatePlatformApplication where
        toQuery CreatePlatformApplication'{..}
          = mconcat
              ["Action" =:
                 ("CreatePlatformApplication" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "Name" =: _cpaName, "Platform" =: _cpaPlatform,
               "Attributes" =:
                 toQueryMap "entry" "key" "value" _cpaAttributes]

-- | Response from CreatePlatformApplication action.
--
-- /See:/ 'createPlatformApplicationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cparsPlatformApplicationARN'
--
-- * 'cparsStatus'
data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse'
    { _cparsPlatformApplicationARN :: !(Maybe Text)
    , _cparsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlatformApplicationResponse' smart constructor.
createPlatformApplicationResponse :: Int -> CreatePlatformApplicationResponse
createPlatformApplicationResponse pStatus_ =
    CreatePlatformApplicationResponse'
    { _cparsPlatformApplicationARN = Nothing
    , _cparsStatus = pStatus_
    }

-- | PlatformApplicationArn is returned.
cparsPlatformApplicationARN :: Lens' CreatePlatformApplicationResponse (Maybe Text)
cparsPlatformApplicationARN = lens _cparsPlatformApplicationARN (\ s a -> s{_cparsPlatformApplicationARN = a});

-- | Undocumented member.
cparsStatus :: Lens' CreatePlatformApplicationResponse Int
cparsStatus = lens _cparsStatus (\ s a -> s{_cparsStatus = a});
