{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a platform application object for one of the supported push
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
-- <http://docs.aws.amazon.com/sns/latest/api/API_CreatePlatformApplication.html>
module Network.AWS.SNS.CreatePlatformApplication
    (
    -- * Request
      CreatePlatformApplication
    -- ** Request constructor
    , createPlatformApplication
    -- ** Request lenses
    , cpaName
    , cpaPlatform
    , cpaAttributes

    -- * Response
    , CreatePlatformApplicationResponse
    -- ** Response constructor
    , createPlatformApplicationResponse
    -- ** Response lenses
    , cparPlatformApplicationARN
    , cparStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

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
    } deriving (Eq,Read,Show)

-- | 'CreatePlatformApplication' smart constructor.
createPlatformApplication :: Text -> Text -> CreatePlatformApplication
createPlatformApplication pName pPlatform =
    CreatePlatformApplication'
    { _cpaName = pName
    , _cpaPlatform = pPlatform
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
        request = post
        response
          = receiveXMLWrapper "CreatePlatformApplicationResult"
              (\ s h x ->
                 CreatePlatformApplicationResponse' <$>
                   (x .@? "PlatformApplicationArn") <*> (pure s))

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
-- * 'cparPlatformApplicationARN'
--
-- * 'cparStatus'
data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse'
    { _cparPlatformApplicationARN :: !(Maybe Text)
    , _cparStatus                 :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreatePlatformApplicationResponse' smart constructor.
createPlatformApplicationResponse :: Status -> CreatePlatformApplicationResponse
createPlatformApplicationResponse pStatus =
    CreatePlatformApplicationResponse'
    { _cparPlatformApplicationARN = Nothing
    , _cparStatus = pStatus
    }

-- | PlatformApplicationArn is returned.
cparPlatformApplicationARN :: Lens' CreatePlatformApplicationResponse (Maybe Text)
cparPlatformApplicationARN = lens _cparPlatformApplicationARN (\ s a -> s{_cparPlatformApplicationARN = a});

-- | FIXME: Undocumented member.
cparStatus :: Lens' CreatePlatformApplicationResponse Status
cparStatus = lens _cparStatus (\ s a -> s{_cparStatus = a});
