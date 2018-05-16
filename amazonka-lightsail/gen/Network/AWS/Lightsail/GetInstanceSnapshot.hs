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
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific instance snapshot.
--
--
module Network.AWS.Lightsail.GetInstanceSnapshot
    (
    -- * Creating a Request
      getInstanceSnapshot
    , GetInstanceSnapshot
    -- * Request Lenses
    , gisInstanceSnapshotName

    -- * Destructuring the Response
    , getInstanceSnapshotResponse
    , GetInstanceSnapshotResponse
    -- * Response Lenses
    , gisrsInstanceSnapshot
    , gisrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceSnapshot' smart constructor.
newtype GetInstanceSnapshot = GetInstanceSnapshot'
  { _gisInstanceSnapshotName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisInstanceSnapshotName' - The name of the snapshot for which you are requesting information.
getInstanceSnapshot
    :: Text -- ^ 'gisInstanceSnapshotName'
    -> GetInstanceSnapshot
getInstanceSnapshot pInstanceSnapshotName_ =
  GetInstanceSnapshot' {_gisInstanceSnapshotName = pInstanceSnapshotName_}


-- | The name of the snapshot for which you are requesting information.
gisInstanceSnapshotName :: Lens' GetInstanceSnapshot Text
gisInstanceSnapshotName = lens _gisInstanceSnapshotName (\ s a -> s{_gisInstanceSnapshotName = a})

instance AWSRequest GetInstanceSnapshot where
        type Rs GetInstanceSnapshot =
             GetInstanceSnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceSnapshotResponse' <$>
                   (x .?> "instanceSnapshot") <*> (pure (fromEnum s)))

instance Hashable GetInstanceSnapshot where

instance NFData GetInstanceSnapshot where

instance ToHeaders GetInstanceSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstanceSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceSnapshot where
        toJSON GetInstanceSnapshot'{..}
          = object
              (catMaybes
                 [Just
                    ("instanceSnapshotName" .=
                       _gisInstanceSnapshotName)])

instance ToPath GetInstanceSnapshot where
        toPath = const "/"

instance ToQuery GetInstanceSnapshot where
        toQuery = const mempty

-- | /See:/ 'getInstanceSnapshotResponse' smart constructor.
data GetInstanceSnapshotResponse = GetInstanceSnapshotResponse'
  { _gisrsInstanceSnapshot :: !(Maybe InstanceSnapshot)
  , _gisrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisrsInstanceSnapshot' - An array of key-value pairs containing information about the results of your get instance snapshot request.
--
-- * 'gisrsResponseStatus' - -- | The response status code.
getInstanceSnapshotResponse
    :: Int -- ^ 'gisrsResponseStatus'
    -> GetInstanceSnapshotResponse
getInstanceSnapshotResponse pResponseStatus_ =
  GetInstanceSnapshotResponse'
    {_gisrsInstanceSnapshot = Nothing, _gisrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your get instance snapshot request.
gisrsInstanceSnapshot :: Lens' GetInstanceSnapshotResponse (Maybe InstanceSnapshot)
gisrsInstanceSnapshot = lens _gisrsInstanceSnapshot (\ s a -> s{_gisrsInstanceSnapshot = a})

-- | -- | The response status code.
gisrsResponseStatus :: Lens' GetInstanceSnapshotResponse Int
gisrsResponseStatus = lens _gisrsResponseStatus (\ s a -> s{_gisrsResponseStatus = a})

instance NFData GetInstanceSnapshotResponse where
