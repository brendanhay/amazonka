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
-- Module      : Network.AWS.Snowball.GetSoftwareUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon S3 presigned URL for an update file associated with a specified @JobId@ .
module Network.AWS.Snowball.GetSoftwareUpdates
  ( -- * Creating a Request
    getSoftwareUpdates,
    GetSoftwareUpdates,

    -- * Request Lenses
    gsuJobId,

    -- * Destructuring the Response
    getSoftwareUpdatesResponse,
    GetSoftwareUpdatesResponse,

    -- * Response Lenses
    gsursUpdatesURI,
    gsursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types

-- | /See:/ 'getSoftwareUpdates' smart constructor.
newtype GetSoftwareUpdates = GetSoftwareUpdates' {_gsuJobId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSoftwareUpdates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsuJobId' - The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
getSoftwareUpdates ::
  -- | 'gsuJobId'
  Text ->
  GetSoftwareUpdates
getSoftwareUpdates pJobId_ =
  GetSoftwareUpdates' {_gsuJobId = pJobId_}

-- | The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
gsuJobId :: Lens' GetSoftwareUpdates Text
gsuJobId = lens _gsuJobId (\s a -> s {_gsuJobId = a})

instance AWSRequest GetSoftwareUpdates where
  type Rs GetSoftwareUpdates = GetSoftwareUpdatesResponse
  request = postJSON snowball
  response =
    receiveJSON
      ( \s h x ->
          GetSoftwareUpdatesResponse'
            <$> (x .?> "UpdatesURI") <*> (pure (fromEnum s))
      )

instance Hashable GetSoftwareUpdates

instance NFData GetSoftwareUpdates

instance ToHeaders GetSoftwareUpdates where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSIESnowballJobManagementService.GetSoftwareUpdates" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSoftwareUpdates where
  toJSON GetSoftwareUpdates' {..} =
    object (catMaybes [Just ("JobId" .= _gsuJobId)])

instance ToPath GetSoftwareUpdates where
  toPath = const "/"

instance ToQuery GetSoftwareUpdates where
  toQuery = const mempty

-- | /See:/ 'getSoftwareUpdatesResponse' smart constructor.
data GetSoftwareUpdatesResponse = GetSoftwareUpdatesResponse'
  { _gsursUpdatesURI ::
      !(Maybe Text),
    _gsursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSoftwareUpdatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsursUpdatesURI' - The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
--
-- * 'gsursResponseStatus' - -- | The response status code.
getSoftwareUpdatesResponse ::
  -- | 'gsursResponseStatus'
  Int ->
  GetSoftwareUpdatesResponse
getSoftwareUpdatesResponse pResponseStatus_ =
  GetSoftwareUpdatesResponse'
    { _gsursUpdatesURI = Nothing,
      _gsursResponseStatus = pResponseStatus_
    }

-- | The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
gsursUpdatesURI :: Lens' GetSoftwareUpdatesResponse (Maybe Text)
gsursUpdatesURI = lens _gsursUpdatesURI (\s a -> s {_gsursUpdatesURI = a})

-- | -- | The response status code.
gsursResponseStatus :: Lens' GetSoftwareUpdatesResponse Int
gsursResponseStatus = lens _gsursResponseStatus (\s a -> s {_gsursResponseStatus = a})

instance NFData GetSoftwareUpdatesResponse
