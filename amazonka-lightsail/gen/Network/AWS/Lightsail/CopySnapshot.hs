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
-- Module      : Network.AWS.Lightsail.CopySnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies an instance or disk snapshot from one AWS Region to another in Amazon Lightsail.
--
--
module Network.AWS.Lightsail.CopySnapshot
    (
    -- * Creating a Request
      copySnapshot
    , CopySnapshot
    -- * Request Lenses
    , csSourceSnapshotName
    , csTargetSnapshotName
    , csSourceRegion

    -- * Destructuring the Response
    , copySnapshotResponse
    , CopySnapshotResponse
    -- * Response Lenses
    , csrsOperations
    , csrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { _csSourceSnapshotName :: !Text
  , _csTargetSnapshotName :: !Text
  , _csSourceRegion       :: !RegionName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSourceSnapshotName' - The name of the source instance or disk snapshot to be copied.
--
-- * 'csTargetSnapshotName' - The name of the new instance or disk snapshot to be created as a copy.
--
-- * 'csSourceRegion' - The AWS Region where the source snapshot is located.
copySnapshot
    :: Text -- ^ 'csSourceSnapshotName'
    -> Text -- ^ 'csTargetSnapshotName'
    -> RegionName -- ^ 'csSourceRegion'
    -> CopySnapshot
copySnapshot pSourceSnapshotName_ pTargetSnapshotName_ pSourceRegion_ =
  CopySnapshot'
    { _csSourceSnapshotName = pSourceSnapshotName_
    , _csTargetSnapshotName = pTargetSnapshotName_
    , _csSourceRegion = pSourceRegion_
    }


-- | The name of the source instance or disk snapshot to be copied.
csSourceSnapshotName :: Lens' CopySnapshot Text
csSourceSnapshotName = lens _csSourceSnapshotName (\ s a -> s{_csSourceSnapshotName = a})

-- | The name of the new instance or disk snapshot to be created as a copy.
csTargetSnapshotName :: Lens' CopySnapshot Text
csTargetSnapshotName = lens _csTargetSnapshotName (\ s a -> s{_csTargetSnapshotName = a})

-- | The AWS Region where the source snapshot is located.
csSourceRegion :: Lens' CopySnapshot RegionName
csSourceRegion = lens _csSourceRegion (\ s a -> s{_csSourceRegion = a})

instance AWSRequest CopySnapshot where
        type Rs CopySnapshot = CopySnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CopySnapshotResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CopySnapshot where

instance NFData CopySnapshot where

instance ToHeaders CopySnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CopySnapshot" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CopySnapshot where
        toJSON CopySnapshot'{..}
          = object
              (catMaybes
                 [Just
                    ("sourceSnapshotName" .= _csSourceSnapshotName),
                  Just ("targetSnapshotName" .= _csTargetSnapshotName),
                  Just ("sourceRegion" .= _csSourceRegion)])

instance ToPath CopySnapshot where
        toPath = const "/"

instance ToQuery CopySnapshot where
        toQuery = const mempty

-- | /See:/ 'copySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { _csrsOperations     :: !(Maybe [Operation])
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsOperations' - A list of objects describing the API operation.
--
-- * 'csrsResponseStatus' - -- | The response status code.
copySnapshotResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CopySnapshotResponse
copySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    {_csrsOperations = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | A list of objects describing the API operation.
csrsOperations :: Lens' CopySnapshotResponse [Operation]
csrsOperations = lens _csrsOperations (\ s a -> s{_csrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
csrsResponseStatus :: Lens' CopySnapshotResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CopySnapshotResponse where
