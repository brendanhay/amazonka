{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.GetSnapshotLimits
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Obtains the manual snapshot limits for a directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_GetSnapshotLimits.html>
module Network.AWS.DirectoryService.GetSnapshotLimits
    (
    -- * Request
      GetSnapshotLimits
    -- ** Request constructor
    , getSnapshotLimits
    -- ** Request lenses
    , gslDirectoryId

    -- * Response
    , GetSnapshotLimitsResponse
    -- ** Response constructor
    , getSnapshotLimitsResponse
    -- ** Response lenses
    , gslrsSnapshotLimits
    , gslrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the GetSnapshotLimits operation.
--
-- /See:/ 'getSnapshotLimits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gslDirectoryId'
newtype GetSnapshotLimits = GetSnapshotLimits'
    { _gslDirectoryId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetSnapshotLimits' smart constructor.
getSnapshotLimits :: Text -> GetSnapshotLimits
getSnapshotLimits pDirectoryId_ =
    GetSnapshotLimits'
    { _gslDirectoryId = pDirectoryId_
    }

-- | Contains the identifier of the directory to obtain the limits for.
gslDirectoryId :: Lens' GetSnapshotLimits Text
gslDirectoryId = lens _gslDirectoryId (\ s a -> s{_gslDirectoryId = a});

instance AWSRequest GetSnapshotLimits where
        type Sv GetSnapshotLimits = DirectoryService
        type Rs GetSnapshotLimits = GetSnapshotLimitsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetSnapshotLimitsResponse' <$>
                   (x .?> "SnapshotLimits") <*> (pure (fromEnum s)))

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
          = object ["DirectoryId" .= _gslDirectoryId]

instance ToPath GetSnapshotLimits where
        toPath = const mempty

instance ToQuery GetSnapshotLimits where
        toQuery = const mempty

-- | Contains the results of the GetSnapshotLimits operation.
--
-- /See:/ 'getSnapshotLimitsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gslrsSnapshotLimits'
--
-- * 'gslrsStatus'
data GetSnapshotLimitsResponse = GetSnapshotLimitsResponse'
    { _gslrsSnapshotLimits :: !(Maybe SnapshotLimits)
    , _gslrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetSnapshotLimitsResponse' smart constructor.
getSnapshotLimitsResponse :: Int -> GetSnapshotLimitsResponse
getSnapshotLimitsResponse pStatus_ =
    GetSnapshotLimitsResponse'
    { _gslrsSnapshotLimits = Nothing
    , _gslrsStatus = pStatus_
    }

-- | A SnapshotLimits object that contains the manual snapshot limits for the
-- specified directory.
gslrsSnapshotLimits :: Lens' GetSnapshotLimitsResponse (Maybe SnapshotLimits)
gslrsSnapshotLimits = lens _gslrsSnapshotLimits (\ s a -> s{_gslrsSnapshotLimits = a});

-- | FIXME: Undocumented member.
gslrsStatus :: Lens' GetSnapshotLimitsResponse Int
gslrsStatus = lens _gslrsStatus (\ s a -> s{_gslrsStatus = a});
