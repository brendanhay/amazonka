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
-- Module      : Network.AWS.MediaLive.DescribeInputDeviceThumbnail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the latest thumbnail data for the input device.
module Network.AWS.MediaLive.DescribeInputDeviceThumbnail
  ( -- * Creating a Request
    describeInputDeviceThumbnail,
    DescribeInputDeviceThumbnail,

    -- * Request Lenses
    didtInputDeviceId,
    didtAccept,

    -- * Destructuring the Response
    describeInputDeviceThumbnailResponse,
    DescribeInputDeviceThumbnailResponse,

    -- * Response Lenses
    didtrsETag,
    didtrsContentLength,
    didtrsLastModified,
    didtrsContentType,
    didtrsResponseStatus,
    didtrsBody,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeInputDeviceThumbnailRequest
--
-- /See:/ 'describeInputDeviceThumbnail' smart constructor.
data DescribeInputDeviceThumbnail = DescribeInputDeviceThumbnail'
  { _didtInputDeviceId ::
      !Text,
    _didtAccept :: !AcceptHeader
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInputDeviceThumbnail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didtInputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
--
-- * 'didtAccept' - The HTTP Accept header. Indicates the requested type for the thumbnail.
describeInputDeviceThumbnail ::
  -- | 'didtInputDeviceId'
  Text ->
  -- | 'didtAccept'
  AcceptHeader ->
  DescribeInputDeviceThumbnail
describeInputDeviceThumbnail pInputDeviceId_ pAccept_ =
  DescribeInputDeviceThumbnail'
    { _didtInputDeviceId =
        pInputDeviceId_,
      _didtAccept = pAccept_
    }

-- | The unique ID of this input device. For example, hd-123456789abcdef.
didtInputDeviceId :: Lens' DescribeInputDeviceThumbnail Text
didtInputDeviceId = lens _didtInputDeviceId (\s a -> s {_didtInputDeviceId = a})

-- | The HTTP Accept header. Indicates the requested type for the thumbnail.
didtAccept :: Lens' DescribeInputDeviceThumbnail AcceptHeader
didtAccept = lens _didtAccept (\s a -> s {_didtAccept = a})

instance AWSRequest DescribeInputDeviceThumbnail where
  type
    Rs DescribeInputDeviceThumbnail =
      DescribeInputDeviceThumbnailResponse
  request = get mediaLive
  response =
    receiveBody
      ( \s h x ->
          DescribeInputDeviceThumbnailResponse'
            <$> (h .#? "ETag")
            <*> (h .#? "Content-Length")
            <*> (h .#? "Last-Modified")
            <*> (h .#? "Content-Type")
            <*> (pure (fromEnum s))
            <*> (pure x)
      )

instance Hashable DescribeInputDeviceThumbnail

instance NFData DescribeInputDeviceThumbnail

instance ToHeaders DescribeInputDeviceThumbnail where
  toHeaders DescribeInputDeviceThumbnail' {..} =
    mconcat
      [ "accept" =# _didtAccept,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToPath DescribeInputDeviceThumbnail where
  toPath DescribeInputDeviceThumbnail' {..} =
    mconcat
      ["/prod/inputDevices/", toBS _didtInputDeviceId, "/thumbnailData"]

instance ToQuery DescribeInputDeviceThumbnail where
  toQuery = const mempty

-- | Placeholder documentation for DescribeInputDeviceThumbnailResponse
--
-- /See:/ 'describeInputDeviceThumbnailResponse' smart constructor.
data DescribeInputDeviceThumbnailResponse = DescribeInputDeviceThumbnailResponse'
  { _didtrsETag ::
      !(Maybe Text),
    _didtrsContentLength ::
      !(Maybe Integer),
    _didtrsLastModified ::
      !(Maybe POSIX),
    _didtrsContentType ::
      !( Maybe
           ContentType
       ),
    _didtrsResponseStatus ::
      !Int,
    _didtrsBody ::
      !RsBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'DescribeInputDeviceThumbnailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didtrsETag' - The unique, cacheable version of this thumbnail.
--
-- * 'didtrsContentLength' - The length of the content.
--
-- * 'didtrsLastModified' - The date and time the thumbnail was last updated at the device.
--
-- * 'didtrsContentType' - Specifies the media type of the thumbnail.
--
-- * 'didtrsResponseStatus' - -- | The response status code.
--
-- * 'didtrsBody' - The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
describeInputDeviceThumbnailResponse ::
  -- | 'didtrsResponseStatus'
  Int ->
  -- | 'didtrsBody'
  RsBody ->
  DescribeInputDeviceThumbnailResponse
describeInputDeviceThumbnailResponse pResponseStatus_ pBody_ =
  DescribeInputDeviceThumbnailResponse'
    { _didtrsETag = Nothing,
      _didtrsContentLength = Nothing,
      _didtrsLastModified = Nothing,
      _didtrsContentType = Nothing,
      _didtrsResponseStatus = pResponseStatus_,
      _didtrsBody = pBody_
    }

-- | The unique, cacheable version of this thumbnail.
didtrsETag :: Lens' DescribeInputDeviceThumbnailResponse (Maybe Text)
didtrsETag = lens _didtrsETag (\s a -> s {_didtrsETag = a})

-- | The length of the content.
didtrsContentLength :: Lens' DescribeInputDeviceThumbnailResponse (Maybe Integer)
didtrsContentLength = lens _didtrsContentLength (\s a -> s {_didtrsContentLength = a})

-- | The date and time the thumbnail was last updated at the device.
didtrsLastModified :: Lens' DescribeInputDeviceThumbnailResponse (Maybe UTCTime)
didtrsLastModified = lens _didtrsLastModified (\s a -> s {_didtrsLastModified = a}) . mapping _Time

-- | Specifies the media type of the thumbnail.
didtrsContentType :: Lens' DescribeInputDeviceThumbnailResponse (Maybe ContentType)
didtrsContentType = lens _didtrsContentType (\s a -> s {_didtrsContentType = a})

-- | -- | The response status code.
didtrsResponseStatus :: Lens' DescribeInputDeviceThumbnailResponse Int
didtrsResponseStatus = lens _didtrsResponseStatus (\s a -> s {_didtrsResponseStatus = a})

-- | The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
didtrsBody :: Lens' DescribeInputDeviceThumbnailResponse RsBody
didtrsBody = lens _didtrsBody (\s a -> s {_didtrsBody = a})
