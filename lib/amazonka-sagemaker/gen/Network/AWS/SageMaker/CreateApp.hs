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
-- Module      : Network.AWS.SageMaker.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a running App for the specified UserProfile. Supported Apps are JupyterServer and KernelGateway. This operation is automatically invoked by Amazon SageMaker Studio upon access to the associated Domain, and when new kernel configurations are selected by the user. A user may have multiple Apps active simultaneously.
module Network.AWS.SageMaker.CreateApp
  ( -- * Creating a Request
    createApp,
    CreateApp,

    -- * Request Lenses
    caResourceSpec,
    caTags,
    caDomainId,
    caUserProfileName,
    caAppType,
    caAppName,

    -- * Destructuring the Response
    createAppResponse,
    CreateAppResponse,

    -- * Response Lenses
    crersAppARN,
    crersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createApp' smart constructor.
data CreateApp = CreateApp'
  { _caResourceSpec ::
      !(Maybe ResourceSpec),
    _caTags :: !(Maybe [Tag]),
    _caDomainId :: !Text,
    _caUserProfileName :: !Text,
    _caAppType :: !AppType,
    _caAppName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caResourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- * 'caTags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- * 'caDomainId' - The domain ID.
--
-- * 'caUserProfileName' - The user profile name.
--
-- * 'caAppType' - The type of app.
--
-- * 'caAppName' - The name of the app.
createApp ::
  -- | 'caDomainId'
  Text ->
  -- | 'caUserProfileName'
  Text ->
  -- | 'caAppType'
  AppType ->
  -- | 'caAppName'
  Text ->
  CreateApp
createApp pDomainId_ pUserProfileName_ pAppType_ pAppName_ =
  CreateApp'
    { _caResourceSpec = Nothing,
      _caTags = Nothing,
      _caDomainId = pDomainId_,
      _caUserProfileName = pUserProfileName_,
      _caAppType = pAppType_,
      _caAppName = pAppName_
    }

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
caResourceSpec :: Lens' CreateApp (Maybe ResourceSpec)
caResourceSpec = lens _caResourceSpec (\s a -> s {_caResourceSpec = a})

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
caTags :: Lens' CreateApp [Tag]
caTags = lens _caTags (\s a -> s {_caTags = a}) . _Default . _Coerce

-- | The domain ID.
caDomainId :: Lens' CreateApp Text
caDomainId = lens _caDomainId (\s a -> s {_caDomainId = a})

-- | The user profile name.
caUserProfileName :: Lens' CreateApp Text
caUserProfileName = lens _caUserProfileName (\s a -> s {_caUserProfileName = a})

-- | The type of app.
caAppType :: Lens' CreateApp AppType
caAppType = lens _caAppType (\s a -> s {_caAppType = a})

-- | The name of the app.
caAppName :: Lens' CreateApp Text
caAppName = lens _caAppName (\s a -> s {_caAppName = a})

instance AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateAppResponse' <$> (x .?> "AppArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateApp

instance NFData CreateApp

instance ToHeaders CreateApp where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateApp" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateApp where
  toJSON CreateApp' {..} =
    object
      ( catMaybes
          [ ("ResourceSpec" .=) <$> _caResourceSpec,
            ("Tags" .=) <$> _caTags,
            Just ("DomainId" .= _caDomainId),
            Just ("UserProfileName" .= _caUserProfileName),
            Just ("AppType" .= _caAppType),
            Just ("AppName" .= _caAppName)
          ]
      )

instance ToPath CreateApp where
  toPath = const "/"

instance ToQuery CreateApp where
  toQuery = const mempty

-- | /See:/ 'createAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { _crersAppARN ::
      !(Maybe Text),
    _crersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersAppARN' - The Amazon Resource Name (ARN) of the app.
--
-- * 'crersResponseStatus' - -- | The response status code.
createAppResponse ::
  -- | 'crersResponseStatus'
  Int ->
  CreateAppResponse
createAppResponse pResponseStatus_ =
  CreateAppResponse'
    { _crersAppARN = Nothing,
      _crersResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the app.
crersAppARN :: Lens' CreateAppResponse (Maybe Text)
crersAppARN = lens _crersAppARN (\s a -> s {_crersAppARN = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateAppResponse Int
crersResponseStatus = lens _crersResponseStatus (\s a -> s {_crersResponseStatus = a})

instance NFData CreateAppResponse
