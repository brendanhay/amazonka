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
-- Module      : Network.AWS.SageMaker.DescribeImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a version of a SageMaker image.
module Network.AWS.SageMaker.DescribeImageVersion
  ( -- * Creating a Request
    describeImageVersion,
    DescribeImageVersion,

    -- * Request Lenses
    dVersion,
    dImageName,

    -- * Destructuring the Response
    describeImageVersionResponse,
    DescribeImageVersionResponse,

    -- * Response Lenses
    divirsCreationTime,
    divirsFailureReason,
    divirsContainerImage,
    divirsLastModifiedTime,
    divirsImageVersionStatus,
    divirsVersion,
    divirsBaseImage,
    divirsImageARN,
    divirsImageVersionARN,
    divirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeImageVersion' smart constructor.
data DescribeImageVersion = DescribeImageVersion'
  { _dVersion ::
      !(Maybe Nat),
    _dImageName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImageVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersion' - The version of the image. If not specified, the latest version is described.
--
-- * 'dImageName' - The name of the image.
describeImageVersion ::
  -- | 'dImageName'
  Text ->
  DescribeImageVersion
describeImageVersion pImageName_ =
  DescribeImageVersion'
    { _dVersion = Nothing,
      _dImageName = pImageName_
    }

-- | The version of the image. If not specified, the latest version is described.
dVersion :: Lens' DescribeImageVersion (Maybe Natural)
dVersion = lens _dVersion (\s a -> s {_dVersion = a}) . mapping _Nat

-- | The name of the image.
dImageName :: Lens' DescribeImageVersion Text
dImageName = lens _dImageName (\s a -> s {_dImageName = a})

instance AWSRequest DescribeImageVersion where
  type Rs DescribeImageVersion = DescribeImageVersionResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeImageVersionResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "FailureReason")
            <*> (x .?> "ContainerImage")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "ImageVersionStatus")
            <*> (x .?> "Version")
            <*> (x .?> "BaseImage")
            <*> (x .?> "ImageArn")
            <*> (x .?> "ImageVersionArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeImageVersion

instance NFData DescribeImageVersion

instance ToHeaders DescribeImageVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeImageVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeImageVersion where
  toJSON DescribeImageVersion' {..} =
    object
      ( catMaybes
          [("Version" .=) <$> _dVersion, Just ("ImageName" .= _dImageName)]
      )

instance ToPath DescribeImageVersion where
  toPath = const "/"

instance ToQuery DescribeImageVersion where
  toQuery = const mempty

-- | /See:/ 'describeImageVersionResponse' smart constructor.
data DescribeImageVersionResponse = DescribeImageVersionResponse'
  { _divirsCreationTime ::
      !(Maybe POSIX),
    _divirsFailureReason ::
      !(Maybe Text),
    _divirsContainerImage ::
      !(Maybe Text),
    _divirsLastModifiedTime ::
      !(Maybe POSIX),
    _divirsImageVersionStatus ::
      !(Maybe ImageVersionStatus),
    _divirsVersion :: !(Maybe Nat),
    _divirsBaseImage :: !(Maybe Text),
    _divirsImageARN :: !(Maybe Text),
    _divirsImageVersionARN ::
      !(Maybe Text),
    _divirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImageVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'divirsCreationTime' - When the version was created.
--
-- * 'divirsFailureReason' - When a create or delete operation fails, the reason for the failure.
--
-- * 'divirsContainerImage' - The registry path of the container image that contains this image version.
--
-- * 'divirsLastModifiedTime' - When the version was last modified.
--
-- * 'divirsImageVersionStatus' - The status of the version.
--
-- * 'divirsVersion' - The version number.
--
-- * 'divirsBaseImage' - The registry path of the container image on which this image version is based.
--
-- * 'divirsImageARN' - The Amazon Resource Name (ARN) of the image the version is based on.
--
-- * 'divirsImageVersionARN' - The ARN of the version.
--
-- * 'divirsResponseStatus' - -- | The response status code.
describeImageVersionResponse ::
  -- | 'divirsResponseStatus'
  Int ->
  DescribeImageVersionResponse
describeImageVersionResponse pResponseStatus_ =
  DescribeImageVersionResponse'
    { _divirsCreationTime = Nothing,
      _divirsFailureReason = Nothing,
      _divirsContainerImage = Nothing,
      _divirsLastModifiedTime = Nothing,
      _divirsImageVersionStatus = Nothing,
      _divirsVersion = Nothing,
      _divirsBaseImage = Nothing,
      _divirsImageARN = Nothing,
      _divirsImageVersionARN = Nothing,
      _divirsResponseStatus = pResponseStatus_
    }

-- | When the version was created.
divirsCreationTime :: Lens' DescribeImageVersionResponse (Maybe UTCTime)
divirsCreationTime = lens _divirsCreationTime (\s a -> s {_divirsCreationTime = a}) . mapping _Time

-- | When a create or delete operation fails, the reason for the failure.
divirsFailureReason :: Lens' DescribeImageVersionResponse (Maybe Text)
divirsFailureReason = lens _divirsFailureReason (\s a -> s {_divirsFailureReason = a})

-- | The registry path of the container image that contains this image version.
divirsContainerImage :: Lens' DescribeImageVersionResponse (Maybe Text)
divirsContainerImage = lens _divirsContainerImage (\s a -> s {_divirsContainerImage = a})

-- | When the version was last modified.
divirsLastModifiedTime :: Lens' DescribeImageVersionResponse (Maybe UTCTime)
divirsLastModifiedTime = lens _divirsLastModifiedTime (\s a -> s {_divirsLastModifiedTime = a}) . mapping _Time

-- | The status of the version.
divirsImageVersionStatus :: Lens' DescribeImageVersionResponse (Maybe ImageVersionStatus)
divirsImageVersionStatus = lens _divirsImageVersionStatus (\s a -> s {_divirsImageVersionStatus = a})

-- | The version number.
divirsVersion :: Lens' DescribeImageVersionResponse (Maybe Natural)
divirsVersion = lens _divirsVersion (\s a -> s {_divirsVersion = a}) . mapping _Nat

-- | The registry path of the container image on which this image version is based.
divirsBaseImage :: Lens' DescribeImageVersionResponse (Maybe Text)
divirsBaseImage = lens _divirsBaseImage (\s a -> s {_divirsBaseImage = a})

-- | The Amazon Resource Name (ARN) of the image the version is based on.
divirsImageARN :: Lens' DescribeImageVersionResponse (Maybe Text)
divirsImageARN = lens _divirsImageARN (\s a -> s {_divirsImageARN = a})

-- | The ARN of the version.
divirsImageVersionARN :: Lens' DescribeImageVersionResponse (Maybe Text)
divirsImageVersionARN = lens _divirsImageVersionARN (\s a -> s {_divirsImageVersionARN = a})

-- | -- | The response status code.
divirsResponseStatus :: Lens' DescribeImageVersionResponse Int
divirsResponseStatus = lens _divirsResponseStatus (\s a -> s {_divirsResponseStatus = a})

instance NFData DescribeImageVersionResponse
