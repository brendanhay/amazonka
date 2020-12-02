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
-- Module      : Network.AWS.Pinpoint.UpdateSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration, dimension, and other settings for an existing segment that's associated with an application.
module Network.AWS.Pinpoint.UpdateSegment
  ( -- * Creating a Request
    updateSegment,
    UpdateSegment,

    -- * Request Lenses
    usSegmentId,
    usApplicationId,
    usWriteSegmentRequest,

    -- * Destructuring the Response
    updateSegmentResponse,
    UpdateSegmentResponse,

    -- * Response Lenses
    usrsResponseStatus,
    usrsSegmentResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSegment' smart constructor.
data UpdateSegment = UpdateSegment'
  { _usSegmentId :: !Text,
    _usApplicationId :: !Text,
    _usWriteSegmentRequest :: !WriteSegmentRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usSegmentId' - The unique identifier for the segment.
--
-- * 'usApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'usWriteSegmentRequest' - Undocumented member.
updateSegment ::
  -- | 'usSegmentId'
  Text ->
  -- | 'usApplicationId'
  Text ->
  -- | 'usWriteSegmentRequest'
  WriteSegmentRequest ->
  UpdateSegment
updateSegment pSegmentId_ pApplicationId_ pWriteSegmentRequest_ =
  UpdateSegment'
    { _usSegmentId = pSegmentId_,
      _usApplicationId = pApplicationId_,
      _usWriteSegmentRequest = pWriteSegmentRequest_
    }

-- | The unique identifier for the segment.
usSegmentId :: Lens' UpdateSegment Text
usSegmentId = lens _usSegmentId (\s a -> s {_usSegmentId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
usApplicationId :: Lens' UpdateSegment Text
usApplicationId = lens _usApplicationId (\s a -> s {_usApplicationId = a})

-- | Undocumented member.
usWriteSegmentRequest :: Lens' UpdateSegment WriteSegmentRequest
usWriteSegmentRequest = lens _usWriteSegmentRequest (\s a -> s {_usWriteSegmentRequest = a})

instance AWSRequest UpdateSegment where
  type Rs UpdateSegment = UpdateSegmentResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateSegmentResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateSegment

instance NFData UpdateSegment

instance ToHeaders UpdateSegment where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateSegment where
  toJSON UpdateSegment' {..} =
    object
      ( catMaybes
          [Just ("WriteSegmentRequest" .= _usWriteSegmentRequest)]
      )

instance ToPath UpdateSegment where
  toPath UpdateSegment' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _usApplicationId,
        "/segments/",
        toBS _usSegmentId
      ]

instance ToQuery UpdateSegment where
  toQuery = const mempty

-- | /See:/ 'updateSegmentResponse' smart constructor.
data UpdateSegmentResponse = UpdateSegmentResponse'
  { _usrsResponseStatus ::
      !Int,
    _usrsSegmentResponse :: !SegmentResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsResponseStatus' - -- | The response status code.
--
-- * 'usrsSegmentResponse' - Undocumented member.
updateSegmentResponse ::
  -- | 'usrsResponseStatus'
  Int ->
  -- | 'usrsSegmentResponse'
  SegmentResponse ->
  UpdateSegmentResponse
updateSegmentResponse pResponseStatus_ pSegmentResponse_ =
  UpdateSegmentResponse'
    { _usrsResponseStatus = pResponseStatus_,
      _usrsSegmentResponse = pSegmentResponse_
    }

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateSegmentResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\s a -> s {_usrsResponseStatus = a})

-- | Undocumented member.
usrsSegmentResponse :: Lens' UpdateSegmentResponse SegmentResponse
usrsSegmentResponse = lens _usrsSegmentResponse (\s a -> s {_usrsSegmentResponse = a})

instance NFData UpdateSegmentResponse
