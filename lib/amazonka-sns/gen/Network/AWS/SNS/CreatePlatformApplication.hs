{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a platform application object for one of the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging), to which devices and mobile apps may register. You must specify @PlatformPrincipal@ and @PlatformCredential@ attributes when using the @CreatePlatformApplication@ action.
--
--
-- @PlatformPrincipal@ and @PlatformCredential@ are received from the notification service.
--
--     * For @ADM@ , @PlatformPrincipal@ is @client id@ and @PlatformCredential@ is @client secret@ .
--
--     * For @Baidu@ , @PlatformPrincipal@ is @API key@ and @PlatformCredential@ is @secret key@ .
--
--     * For @APNS@ and @APNS_SANDBOX@ , @PlatformPrincipal@ is @SSL certificate@ and @PlatformCredential@ is @private key@ .
--
--     * For @GCM@ (Firebase Cloud Messaging), there is no @PlatformPrincipal@ and the @PlatformCredential@ is @API key@ .
--
--     * For @MPNS@ , @PlatformPrincipal@ is @TLS certificate@ and @PlatformCredential@ is @private key@ .
--
--     * For @WNS@ , @PlatformPrincipal@ is @Package Security Identifier@ and @PlatformCredential@ is @secret key@ .
--
--
--
-- You can use the returned @PlatformApplicationArn@ as an attribute for the @CreatePlatformEndpoint@ action.
module Network.AWS.SNS.CreatePlatformApplication
  ( -- * Creating a Request
    createPlatformApplication,
    CreatePlatformApplication,

    -- * Request Lenses
    cpaName,
    cpaPlatform,
    cpaAttributes,

    -- * Destructuring the Response
    createPlatformApplicationResponse,
    CreatePlatformApplicationResponse,

    -- * Response Lenses
    cparsPlatformApplicationARN,
    cparsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for CreatePlatformApplication action.
--
--
--
-- /See:/ 'createPlatformApplication' smart constructor.
data CreatePlatformApplication = CreatePlatformApplication'
  { _cpaName ::
      !Text,
    _cpaPlatform :: !Text,
    _cpaAttributes :: !(Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePlatformApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpaName' - Application names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, hyphens, and periods, and must be between 1 and 256 characters long.
--
-- * 'cpaPlatform' - The following platforms are supported: ADM (Amazon Device Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase Cloud Messaging).
--
-- * 'cpaAttributes' - For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>
createPlatformApplication ::
  -- | 'cpaName'
  Text ->
  -- | 'cpaPlatform'
  Text ->
  CreatePlatformApplication
createPlatformApplication pName_ pPlatform_ =
  CreatePlatformApplication'
    { _cpaName = pName_,
      _cpaPlatform = pPlatform_,
      _cpaAttributes = mempty
    }

-- | Application names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, hyphens, and periods, and must be between 1 and 256 characters long.
cpaName :: Lens' CreatePlatformApplication Text
cpaName = lens _cpaName (\s a -> s {_cpaName = a})

-- | The following platforms are supported: ADM (Amazon Device Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase Cloud Messaging).
cpaPlatform :: Lens' CreatePlatformApplication Text
cpaPlatform = lens _cpaPlatform (\s a -> s {_cpaPlatform = a})

-- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>
cpaAttributes :: Lens' CreatePlatformApplication (HashMap Text (Text))
cpaAttributes = lens _cpaAttributes (\s a -> s {_cpaAttributes = a}) . _Map

instance AWSRequest CreatePlatformApplication where
  type
    Rs CreatePlatformApplication =
      CreatePlatformApplicationResponse
  request = postQuery sns
  response =
    receiveXMLWrapper
      "CreatePlatformApplicationResult"
      ( \s h x ->
          CreatePlatformApplicationResponse'
            <$> (x .@? "PlatformApplicationArn") <*> (pure (fromEnum s))
      )

instance Hashable CreatePlatformApplication

instance NFData CreatePlatformApplication

instance ToHeaders CreatePlatformApplication where
  toHeaders = const mempty

instance ToPath CreatePlatformApplication where
  toPath = const "/"

instance ToQuery CreatePlatformApplication where
  toQuery CreatePlatformApplication' {..} =
    mconcat
      [ "Action" =: ("CreatePlatformApplication" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "Name" =: _cpaName,
        "Platform" =: _cpaPlatform,
        "Attributes" =: toQueryMap "entry" "key" "value" _cpaAttributes
      ]

-- | Response from CreatePlatformApplication action.
--
--
--
-- /See:/ 'createPlatformApplicationResponse' smart constructor.
data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse'
  { _cparsPlatformApplicationARN ::
      !(Maybe Text),
    _cparsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePlatformApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cparsPlatformApplicationARN' - PlatformApplicationArn is returned.
--
-- * 'cparsResponseStatus' - -- | The response status code.
createPlatformApplicationResponse ::
  -- | 'cparsResponseStatus'
  Int ->
  CreatePlatformApplicationResponse
createPlatformApplicationResponse pResponseStatus_ =
  CreatePlatformApplicationResponse'
    { _cparsPlatformApplicationARN =
        Nothing,
      _cparsResponseStatus = pResponseStatus_
    }

-- | PlatformApplicationArn is returned.
cparsPlatformApplicationARN :: Lens' CreatePlatformApplicationResponse (Maybe Text)
cparsPlatformApplicationARN = lens _cparsPlatformApplicationARN (\s a -> s {_cparsPlatformApplicationARN = a})

-- | -- | The response status code.
cparsResponseStatus :: Lens' CreatePlatformApplicationResponse Int
cparsResponseStatus = lens _cparsResponseStatus (\s a -> s {_cparsResponseStatus = a})

instance NFData CreatePlatformApplicationResponse
