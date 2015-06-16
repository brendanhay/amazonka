{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.CopySnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /CopySnapshot/ action makes a copy of an existing snapshot.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CopySnapshot.html>
module Network.AWS.ElastiCache.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , copySnapshot
    -- ** Request lenses
    , csSourceSnapshotName
    , csTargetSnapshotName

    -- * Response
    , CopySnapshotResponse
    -- ** Response constructor
    , copySnapshotResponse
    -- ** Response lenses
    , csrSnapshot
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElastiCache.Types

-- | /See:/ 'copySnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csSourceSnapshotName'
--
-- * 'csTargetSnapshotName'
data CopySnapshot = CopySnapshot'{_csSourceSnapshotName :: Text, _csTargetSnapshotName :: Text} deriving (Eq, Read, Show)

-- | 'CopySnapshot' smart constructor.
copySnapshot :: Text -> Text -> CopySnapshot
copySnapshot pSourceSnapshotName pTargetSnapshotName = CopySnapshot'{_csSourceSnapshotName = pSourceSnapshotName, _csTargetSnapshotName = pTargetSnapshotName};

-- | The name of an existing snapshot from which to copy.
csSourceSnapshotName :: Lens' CopySnapshot Text
csSourceSnapshotName = lens _csSourceSnapshotName (\ s a -> s{_csSourceSnapshotName = a});

-- | A name for the copied snapshot.
csTargetSnapshotName :: Lens' CopySnapshot Text
csTargetSnapshotName = lens _csTargetSnapshotName (\ s a -> s{_csTargetSnapshotName = a});

instance AWSRequest CopySnapshot where
        type Sv CopySnapshot = ElastiCache
        type Rs CopySnapshot = CopySnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CopySnapshotResult"
              (\ s h x ->
                 CopySnapshotResponse' <$> (x .@? "Snapshot"))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSnapshot'
newtype CopySnapshotResponse = CopySnapshotResponse'{_csrSnapshot :: Maybe Snapshot} deriving (Eq, Read, Show)

-- | 'CopySnapshotResponse' smart constructor.
copySnapshotResponse :: CopySnapshotResponse
copySnapshotResponse = CopySnapshotResponse'{_csrSnapshot = Nothing};

-- | FIXME: Undocumented member.
csrSnapshot :: Lens' CopySnapshotResponse (Maybe Snapshot)
csrSnapshot = lens _csrSnapshot (\ s a -> s{_csrSnapshot = a});
