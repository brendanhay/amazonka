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
-- Module      : Network.AWS.ElastiCache.CopySnapshot
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /CopySnapshot/ action makes a copy of an existing snapshot.
module Network.AWS.ElastiCache.CopySnapshot
    (
    -- * Creating a Request
      copySnapshot
    , CopySnapshot
    -- * Request Lenses
    , csSourceSnapshotName
    , csTargetSnapshotName

    -- * Destructuring the Response
    , copySnapshotResponse
    , CopySnapshotResponse
    -- * Response Lenses
    , csrsSnapshot
    , csrsResponseStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CopySnapshotMessage/ action.
--
-- /See:/ 'copySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
    { _csSourceSnapshotName :: !Text
    , _csTargetSnapshotName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSourceSnapshotName'
--
-- * 'csTargetSnapshotName'
copySnapshot
    :: Text -- ^ 'csSourceSnapshotName'
    -> Text -- ^ 'csTargetSnapshotName'
    -> CopySnapshot
copySnapshot pSourceSnapshotName_ pTargetSnapshotName_ =
    CopySnapshot'
    { _csSourceSnapshotName = pSourceSnapshotName_
    , _csTargetSnapshotName = pTargetSnapshotName_
    }

-- | The name of an existing snapshot from which to copy.
csSourceSnapshotName :: Lens' CopySnapshot Text
csSourceSnapshotName = lens _csSourceSnapshotName (\ s a -> s{_csSourceSnapshotName = a});

-- | A name for the copied snapshot.
csTargetSnapshotName :: Lens' CopySnapshot Text
csTargetSnapshotName = lens _csTargetSnapshotName (\ s a -> s{_csTargetSnapshotName = a});

instance AWSRequest CopySnapshot where
        type Rs CopySnapshot = CopySnapshotResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "CopySnapshotResult"
              (\ s h x ->
                 CopySnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance Hashable CopySnapshot

instance NFData CopySnapshot

instance ToHeaders CopySnapshot where
        toHeaders = const mempty

instance ToPath CopySnapshot where
        toPath = const "/"

instance ToQuery CopySnapshot where
        toQuery CopySnapshot'{..}
          = mconcat
              ["Action" =: ("CopySnapshot" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "SourceSnapshotName" =: _csSourceSnapshotName,
               "TargetSnapshotName" =: _csTargetSnapshotName]

-- | /See:/ 'copySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
    { _csrsSnapshot       :: !(Maybe Snapshot)
    , _csrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsSnapshot'
--
-- * 'csrsResponseStatus'
copySnapshotResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CopySnapshotResponse
copySnapshotResponse pResponseStatus_ =
    CopySnapshotResponse'
    { _csrsSnapshot = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
csrsSnapshot :: Lens' CopySnapshotResponse (Maybe Snapshot)
csrsSnapshot = lens _csrsSnapshot (\ s a -> s{_csrsSnapshot = a});

-- | The response status code.
csrsResponseStatus :: Lens' CopySnapshotResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a});
