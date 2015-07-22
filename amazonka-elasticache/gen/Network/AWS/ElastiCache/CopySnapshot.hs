{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CopySnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /CopySnapshot/ action makes a copy of an existing snapshot.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CopySnapshot.html>
module Network.AWS.ElastiCache.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , copySnapshot
    -- ** Request lenses
    , csrqSourceSnapshotName
    , csrqTargetSnapshotName

    -- * Response
    , CopySnapshotResponse
    -- ** Response constructor
    , copySnapshotResponse
    -- ** Response lenses
    , csrsSnapshot
    , csrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CopySnapshotMessage/ action.
--
-- /See:/ 'copySnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrqSourceSnapshotName'
--
-- * 'csrqTargetSnapshotName'
data CopySnapshot = CopySnapshot'
    { _csrqSourceSnapshotName :: !Text
    , _csrqTargetSnapshotName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopySnapshot' smart constructor.
copySnapshot :: Text -> Text -> CopySnapshot
copySnapshot pSourceSnapshotName pTargetSnapshotName =
    CopySnapshot'
    { _csrqSourceSnapshotName = pSourceSnapshotName
    , _csrqTargetSnapshotName = pTargetSnapshotName
    }

-- | The name of an existing snapshot from which to copy.
csrqSourceSnapshotName :: Lens' CopySnapshot Text
csrqSourceSnapshotName = lens _csrqSourceSnapshotName (\ s a -> s{_csrqSourceSnapshotName = a});

-- | A name for the copied snapshot.
csrqTargetSnapshotName :: Lens' CopySnapshot Text
csrqTargetSnapshotName = lens _csrqTargetSnapshotName (\ s a -> s{_csrqTargetSnapshotName = a});

instance AWSRequest CopySnapshot where
        type Sv CopySnapshot = ElastiCache
        type Rs CopySnapshot = CopySnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CopySnapshotResult"
              (\ s h x ->
                 CopySnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance ToHeaders CopySnapshot where
        toHeaders = const mempty

instance ToPath CopySnapshot where
        toPath = const "/"

instance ToQuery CopySnapshot where
        toQuery CopySnapshot'{..}
          = mconcat
              ["Action" =: ("CopySnapshot" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "SourceSnapshotName" =: _csrqSourceSnapshotName,
               "TargetSnapshotName" =: _csrqTargetSnapshotName]

-- | /See:/ 'copySnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsSnapshot'
--
-- * 'csrsStatus'
data CopySnapshotResponse = CopySnapshotResponse'
    { _csrsSnapshot :: !(Maybe Snapshot)
    , _csrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopySnapshotResponse' smart constructor.
copySnapshotResponse :: Int -> CopySnapshotResponse
copySnapshotResponse pStatus =
    CopySnapshotResponse'
    { _csrsSnapshot = Nothing
    , _csrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
csrsSnapshot :: Lens' CopySnapshotResponse (Maybe Snapshot)
csrsSnapshot = lens _csrsSnapshot (\ s a -> s{_csrsSnapshot = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CopySnapshotResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
