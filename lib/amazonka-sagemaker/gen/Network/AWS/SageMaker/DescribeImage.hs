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
-- Module      : Network.AWS.SageMaker.DescribeImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SageMaker image.
module Network.AWS.SageMaker.DescribeImage
  ( -- * Creating a Request
    describeImage,
    DescribeImage,

    -- * Request Lenses
    diImageName,

    -- * Destructuring the Response
    describeImageResponse,
    DescribeImageResponse,

    -- * Response Lenses
    desrsCreationTime,
    desrsFailureReason,
    desrsImageStatus,
    desrsLastModifiedTime,
    desrsImageARN,
    desrsDisplayName,
    desrsImageName,
    desrsDescription,
    desrsRoleARN,
    desrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeImage' smart constructor.
newtype DescribeImage = DescribeImage' {_diImageName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diImageName' - The name of the image to describe.
describeImage ::
  -- | 'diImageName'
  Text ->
  DescribeImage
describeImage pImageName_ =
  DescribeImage' {_diImageName = pImageName_}

-- | The name of the image to describe.
diImageName :: Lens' DescribeImage Text
diImageName = lens _diImageName (\s a -> s {_diImageName = a})

instance AWSRequest DescribeImage where
  type Rs DescribeImage = DescribeImageResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeImageResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "FailureReason")
            <*> (x .?> "ImageStatus")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "ImageArn")
            <*> (x .?> "DisplayName")
            <*> (x .?> "ImageName")
            <*> (x .?> "Description")
            <*> (x .?> "RoleArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeImage

instance NFData DescribeImage

instance ToHeaders DescribeImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeImage where
  toJSON DescribeImage' {..} =
    object (catMaybes [Just ("ImageName" .= _diImageName)])

instance ToPath DescribeImage where
  toPath = const "/"

instance ToQuery DescribeImage where
  toQuery = const mempty

-- | /See:/ 'describeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { _desrsCreationTime ::
      !(Maybe POSIX),
    _desrsFailureReason :: !(Maybe Text),
    _desrsImageStatus :: !(Maybe ImageStatus),
    _desrsLastModifiedTime :: !(Maybe POSIX),
    _desrsImageARN :: !(Maybe Text),
    _desrsDisplayName :: !(Maybe Text),
    _desrsImageName :: !(Maybe Text),
    _desrsDescription :: !(Maybe Text),
    _desrsRoleARN :: !(Maybe Text),
    _desrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsCreationTime' - When the image was created.
--
-- * 'desrsFailureReason' - When a create, update, or delete operation fails, the reason for the failure.
--
-- * 'desrsImageStatus' - The status of the image.
--
-- * 'desrsLastModifiedTime' - When the image was last modified.
--
-- * 'desrsImageARN' - The Amazon Resource Name (ARN) of the image.
--
-- * 'desrsDisplayName' - The name of the image as displayed.
--
-- * 'desrsImageName' - The name of the image.
--
-- * 'desrsDescription' - The description of the image.
--
-- * 'desrsRoleARN' - The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeImageResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeImageResponse
describeImageResponse pResponseStatus_ =
  DescribeImageResponse'
    { _desrsCreationTime = Nothing,
      _desrsFailureReason = Nothing,
      _desrsImageStatus = Nothing,
      _desrsLastModifiedTime = Nothing,
      _desrsImageARN = Nothing,
      _desrsDisplayName = Nothing,
      _desrsImageName = Nothing,
      _desrsDescription = Nothing,
      _desrsRoleARN = Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | When the image was created.
desrsCreationTime :: Lens' DescribeImageResponse (Maybe UTCTime)
desrsCreationTime = lens _desrsCreationTime (\s a -> s {_desrsCreationTime = a}) . mapping _Time

-- | When a create, update, or delete operation fails, the reason for the failure.
desrsFailureReason :: Lens' DescribeImageResponse (Maybe Text)
desrsFailureReason = lens _desrsFailureReason (\s a -> s {_desrsFailureReason = a})

-- | The status of the image.
desrsImageStatus :: Lens' DescribeImageResponse (Maybe ImageStatus)
desrsImageStatus = lens _desrsImageStatus (\s a -> s {_desrsImageStatus = a})

-- | When the image was last modified.
desrsLastModifiedTime :: Lens' DescribeImageResponse (Maybe UTCTime)
desrsLastModifiedTime = lens _desrsLastModifiedTime (\s a -> s {_desrsLastModifiedTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the image.
desrsImageARN :: Lens' DescribeImageResponse (Maybe Text)
desrsImageARN = lens _desrsImageARN (\s a -> s {_desrsImageARN = a})

-- | The name of the image as displayed.
desrsDisplayName :: Lens' DescribeImageResponse (Maybe Text)
desrsDisplayName = lens _desrsDisplayName (\s a -> s {_desrsDisplayName = a})

-- | The name of the image.
desrsImageName :: Lens' DescribeImageResponse (Maybe Text)
desrsImageName = lens _desrsImageName (\s a -> s {_desrsImageName = a})

-- | The description of the image.
desrsDescription :: Lens' DescribeImageResponse (Maybe Text)
desrsDescription = lens _desrsDescription (\s a -> s {_desrsDescription = a})

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
desrsRoleARN :: Lens' DescribeImageResponse (Maybe Text)
desrsRoleARN = lens _desrsRoleARN (\s a -> s {_desrsRoleARN = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeImageResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeImageResponse
