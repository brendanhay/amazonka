{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.GetSnapshotLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains the manual snapshot limits for a directory.
--
--
module Network.AWS.DirectoryService.GetSnapshotLimits
    (
    -- * Creating a Request
      getSnapshotLimits
    , GetSnapshotLimits
    -- * Request Lenses
    , gslDirectoryId

    -- * Destructuring the Response
    , getSnapshotLimitsResponse
    , GetSnapshotLimitsResponse
    -- * Response Lenses
    , gslrsSnapshotLimits
    , gslrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'GetSnapshotLimits' operation.
--
--
--
-- /See:/ 'getSnapshotLimits' smart constructor.
newtype GetSnapshotLimits = GetSnapshotLimits'
  { _gslDirectoryId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSnapshotLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gslDirectoryId' - Contains the identifier of the directory to obtain the limits for.
getSnapshotLimits
    :: Text -- ^ 'gslDirectoryId'
    -> GetSnapshotLimits
getSnapshotLimits pDirectoryId_ =
  GetSnapshotLimits' {_gslDirectoryId = pDirectoryId_}


-- | Contains the identifier of the directory to obtain the limits for.
gslDirectoryId :: Lens' GetSnapshotLimits Text
gslDirectoryId = lens _gslDirectoryId (\ s a -> s{_gslDirectoryId = a})

instance AWSRequest GetSnapshotLimits where
        type Rs GetSnapshotLimits = GetSnapshotLimitsResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 GetSnapshotLimitsResponse' <$>
                   (x .?> "SnapshotLimits") <*> (pure (fromEnum s)))

instance Hashable GetSnapshotLimits where

instance NFData GetSnapshotLimits where

instance ToHeaders GetSnapshotLimits where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.GetSnapshotLimits" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSnapshotLimits where
        toJSON GetSnapshotLimits'{..}
          = object
              (catMaybes [Just ("DirectoryId" .= _gslDirectoryId)])

instance ToPath GetSnapshotLimits where
        toPath = const "/"

instance ToQuery GetSnapshotLimits where
        toQuery = const mempty

-- | Contains the results of the 'GetSnapshotLimits' operation.
--
--
--
-- /See:/ 'getSnapshotLimitsResponse' smart constructor.
data GetSnapshotLimitsResponse = GetSnapshotLimitsResponse'
  { _gslrsSnapshotLimits :: !(Maybe SnapshotLimits)
  , _gslrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSnapshotLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gslrsSnapshotLimits' - A 'SnapshotLimits' object that contains the manual snapshot limits for the specified directory.
--
-- * 'gslrsResponseStatus' - -- | The response status code.
getSnapshotLimitsResponse
    :: Int -- ^ 'gslrsResponseStatus'
    -> GetSnapshotLimitsResponse
getSnapshotLimitsResponse pResponseStatus_ =
  GetSnapshotLimitsResponse'
    {_gslrsSnapshotLimits = Nothing, _gslrsResponseStatus = pResponseStatus_}


-- | A 'SnapshotLimits' object that contains the manual snapshot limits for the specified directory.
gslrsSnapshotLimits :: Lens' GetSnapshotLimitsResponse (Maybe SnapshotLimits)
gslrsSnapshotLimits = lens _gslrsSnapshotLimits (\ s a -> s{_gslrsSnapshotLimits = a})

-- | -- | The response status code.
gslrsResponseStatus :: Lens' GetSnapshotLimitsResponse Int
gslrsResponseStatus = lens _gslrsResponseStatus (\ s a -> s{_gslrsResponseStatus = a})

instance NFData GetSnapshotLimitsResponse where
