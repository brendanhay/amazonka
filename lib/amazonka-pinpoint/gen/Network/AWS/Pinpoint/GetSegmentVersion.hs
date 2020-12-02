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
-- Module      : Network.AWS.Pinpoint.GetSegmentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for a specific version of a segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegmentVersion
  ( -- * Creating a Request
    getSegmentVersion,
    GetSegmentVersion,

    -- * Request Lenses
    gSegmentId,
    gVersion,
    gApplicationId,

    -- * Destructuring the Response
    getSegmentVersionResponse,
    GetSegmentVersionResponse,

    -- * Response Lenses
    gsvrsResponseStatus,
    gsvrsSegmentResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { _gSegmentId :: !Text,
    _gVersion :: !Text,
    _gApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gSegmentId' - The unique identifier for the segment.
--
-- * 'gVersion' - The unique version number (Version property) for the campaign version.
--
-- * 'gApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getSegmentVersion ::
  -- | 'gSegmentId'
  Text ->
  -- | 'gVersion'
  Text ->
  -- | 'gApplicationId'
  Text ->
  GetSegmentVersion
getSegmentVersion pSegmentId_ pVersion_ pApplicationId_ =
  GetSegmentVersion'
    { _gSegmentId = pSegmentId_,
      _gVersion = pVersion_,
      _gApplicationId = pApplicationId_
    }

-- | The unique identifier for the segment.
gSegmentId :: Lens' GetSegmentVersion Text
gSegmentId = lens _gSegmentId (\s a -> s {_gSegmentId = a})

-- | The unique version number (Version property) for the campaign version.
gVersion :: Lens' GetSegmentVersion Text
gVersion = lens _gVersion (\s a -> s {_gVersion = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gApplicationId :: Lens' GetSegmentVersion Text
gApplicationId = lens _gApplicationId (\s a -> s {_gApplicationId = a})

instance AWSRequest GetSegmentVersion where
  type Rs GetSegmentVersion = GetSegmentVersionResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetSegmentVersionResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetSegmentVersion

instance NFData GetSegmentVersion

instance ToHeaders GetSegmentVersion where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetSegmentVersion where
  toPath GetSegmentVersion' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gApplicationId,
        "/segments/",
        toBS _gSegmentId,
        "/versions/",
        toBS _gVersion
      ]

instance ToQuery GetSegmentVersion where
  toQuery = const mempty

-- | /See:/ 'getSegmentVersionResponse' smart constructor.
data GetSegmentVersionResponse = GetSegmentVersionResponse'
  { _gsvrsResponseStatus ::
      !Int,
    _gsvrsSegmentResponse ::
      !SegmentResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvrsResponseStatus' - -- | The response status code.
--
-- * 'gsvrsSegmentResponse' - Undocumented member.
getSegmentVersionResponse ::
  -- | 'gsvrsResponseStatus'
  Int ->
  -- | 'gsvrsSegmentResponse'
  SegmentResponse ->
  GetSegmentVersionResponse
getSegmentVersionResponse pResponseStatus_ pSegmentResponse_ =
  GetSegmentVersionResponse'
    { _gsvrsResponseStatus =
        pResponseStatus_,
      _gsvrsSegmentResponse = pSegmentResponse_
    }

-- | -- | The response status code.
gsvrsResponseStatus :: Lens' GetSegmentVersionResponse Int
gsvrsResponseStatus = lens _gsvrsResponseStatus (\s a -> s {_gsvrsResponseStatus = a})

-- | Undocumented member.
gsvrsSegmentResponse :: Lens' GetSegmentVersionResponse SegmentResponse
gsvrsSegmentResponse = lens _gsvrsSegmentResponse (\s a -> s {_gsvrsSegmentResponse = a})

instance NFData GetSegmentVersionResponse
