{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/sns/latest/api/API_CreatePlatformApplication.html>
module Network.AWS.SNS.CreatePlatformApplication
    (
    -- * Request
      CreatePlatformApplication
    -- ** Request constructor
    , createPlatformApplication
    -- ** Request lenses
    , cparqName
    , cparqPlatform
    , cparqAttributes

    -- * Response
    , CreatePlatformApplicationResponse
    -- ** Response constructor
    , createPlatformApplicationResponse
    -- ** Response lenses
    , cparsPlatformApplicationARN
    , cparsStatus
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
-- * 'cparqName'
--
-- * 'cparqPlatform'
--
-- * 'cparqAttributes'
data CreatePlatformApplication = CreatePlatformApplication'
    { _cparqName       :: !Text
    , _cparqPlatform   :: !Text
    , _cparqAttributes :: !(Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlatformApplication' smart constructor.
createPlatformApplication :: Text -> Text -> CreatePlatformApplication
createPlatformApplication pName pPlatform =
    CreatePlatformApplication'
    { _cparqName = pName
    , _cparqPlatform = pPlatform
    , _cparqAttributes = mempty
    }

-- | Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between
-- 1 and 256 characters long.
cparqName :: Lens' CreatePlatformApplication Text
cparqName = lens _cparqName (\ s a -> s{_cparqName = a});

-- | The following platforms are supported: ADM (Amazon Device Messaging),
-- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Google
-- Cloud Messaging).
cparqPlatform :: Lens' CreatePlatformApplication Text
cparqPlatform = lens _cparqPlatform (\ s a -> s{_cparqPlatform = a});

-- | For a list of attributes, see
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>
cparqAttributes :: Lens' CreatePlatformApplication (HashMap Text Text)
cparqAttributes = lens _cparqAttributes (\ s a -> s{_cparqAttributes = a}) . _Map;

instance AWSRequest CreatePlatformApplication where
        type Sv CreatePlatformApplication = SNS
        type Rs CreatePlatformApplication =
             CreatePlatformApplicationResponse
        request = post
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
               "Name" =: _cparqName, "Platform" =: _cparqPlatform,
               "Attributes" =:
                 toQueryMap "entry" "key" "value" _cparqAttributes]

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
createPlatformApplicationResponse pStatus =
    CreatePlatformApplicationResponse'
    { _cparsPlatformApplicationARN = Nothing
    , _cparsStatus = pStatus
    }

-- | PlatformApplicationArn is returned.
cparsPlatformApplicationARN :: Lens' CreatePlatformApplicationResponse (Maybe Text)
cparsPlatformApplicationARN = lens _cparsPlatformApplicationARN (\ s a -> s{_cparsPlatformApplicationARN = a});

-- | FIXME: Undocumented member.
cparsStatus :: Lens' CreatePlatformApplicationResponse Int
cparsStatus = lens _cparsStatus (\ s a -> s{_cparsStatus = a});
