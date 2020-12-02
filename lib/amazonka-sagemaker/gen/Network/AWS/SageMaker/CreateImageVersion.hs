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
-- Module      : Network.AWS.SageMaker.CreateImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of the SageMaker image specified by @ImageName@ . The version represents the Amazon Container Registry (ECR) container image specified by @BaseImage@ .
module Network.AWS.SageMaker.CreateImageVersion
  ( -- * Creating a Request
    createImageVersion,
    CreateImageVersion,

    -- * Request Lenses
    civBaseImage,
    civClientToken,
    civImageName,

    -- * Destructuring the Response
    createImageVersionResponse,
    CreateImageVersionResponse,

    -- * Response Lenses
    civrsImageVersionARN,
    civrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createImageVersion' smart constructor.
data CreateImageVersion = CreateImageVersion'
  { _civBaseImage ::
      !Text,
    _civClientToken :: !Text,
    _civImageName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateImageVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'civBaseImage' - The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format: @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
--
-- * 'civClientToken' - A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
--
-- * 'civImageName' - The @ImageName@ of the @Image@ to create a version of.
createImageVersion ::
  -- | 'civBaseImage'
  Text ->
  -- | 'civClientToken'
  Text ->
  -- | 'civImageName'
  Text ->
  CreateImageVersion
createImageVersion pBaseImage_ pClientToken_ pImageName_ =
  CreateImageVersion'
    { _civBaseImage = pBaseImage_,
      _civClientToken = pClientToken_,
      _civImageName = pImageName_
    }

-- | The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format: @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
civBaseImage :: Lens' CreateImageVersion Text
civBaseImage = lens _civBaseImage (\s a -> s {_civBaseImage = a})

-- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
civClientToken :: Lens' CreateImageVersion Text
civClientToken = lens _civClientToken (\s a -> s {_civClientToken = a})

-- | The @ImageName@ of the @Image@ to create a version of.
civImageName :: Lens' CreateImageVersion Text
civImageName = lens _civImageName (\s a -> s {_civImageName = a})

instance AWSRequest CreateImageVersion where
  type Rs CreateImageVersion = CreateImageVersionResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateImageVersionResponse'
            <$> (x .?> "ImageVersionArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateImageVersion

instance NFData CreateImageVersion

instance ToHeaders CreateImageVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateImageVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateImageVersion where
  toJSON CreateImageVersion' {..} =
    object
      ( catMaybes
          [ Just ("BaseImage" .= _civBaseImage),
            Just ("ClientToken" .= _civClientToken),
            Just ("ImageName" .= _civImageName)
          ]
      )

instance ToPath CreateImageVersion where
  toPath = const "/"

instance ToQuery CreateImageVersion where
  toQuery = const mempty

-- | /See:/ 'createImageVersionResponse' smart constructor.
data CreateImageVersionResponse = CreateImageVersionResponse'
  { _civrsImageVersionARN ::
      !(Maybe Text),
    _civrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateImageVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'civrsImageVersionARN' - The Amazon Resource Name (ARN) of the image version.
--
-- * 'civrsResponseStatus' - -- | The response status code.
createImageVersionResponse ::
  -- | 'civrsResponseStatus'
  Int ->
  CreateImageVersionResponse
createImageVersionResponse pResponseStatus_ =
  CreateImageVersionResponse'
    { _civrsImageVersionARN = Nothing,
      _civrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the image version.
civrsImageVersionARN :: Lens' CreateImageVersionResponse (Maybe Text)
civrsImageVersionARN = lens _civrsImageVersionARN (\s a -> s {_civrsImageVersionARN = a})

-- | -- | The response status code.
civrsResponseStatus :: Lens' CreateImageVersionResponse Int
civrsResponseStatus = lens _civrsResponseStatus (\s a -> s {_civrsResponseStatus = a})

instance NFData CreateImageVersionResponse
