{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Return torrent files from a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObjectTorrent.html>
module Network.AWS.S3.GetObjectTorrent
    (
    -- * Request
      GetObjectTorrent
    -- ** Request constructor
    , getObjectTorrent
    -- ** Request lenses
    , gotrqRequestPayer
    , gotrqBucket
    , gotrqKey

    -- * Response
    , GetObjectTorrentResponse
    -- ** Response constructor
    , getObjectTorrentResponse
    -- ** Response lenses
    , gotrsRequestCharged
    , gotrsStatus
    , gotrsBody
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getObjectTorrent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotrqRequestPayer'
--
-- * 'gotrqBucket'
--
-- * 'gotrqKey'
data GetObjectTorrent = GetObjectTorrent'
    { _gotrqRequestPayer :: !(Maybe RequestPayer)
    , _gotrqBucket       :: !BucketName
    , _gotrqKey          :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetObjectTorrent' smart constructor.
getObjectTorrent :: BucketName -> ObjectKey -> GetObjectTorrent
getObjectTorrent pBucket pKey =
    GetObjectTorrent'
    { _gotrqRequestPayer = Nothing
    , _gotrqBucket = pBucket
    , _gotrqKey = pKey
    }

-- | FIXME: Undocumented member.
gotrqRequestPayer :: Lens' GetObjectTorrent (Maybe RequestPayer)
gotrqRequestPayer = lens _gotrqRequestPayer (\ s a -> s{_gotrqRequestPayer = a});

-- | FIXME: Undocumented member.
gotrqBucket :: Lens' GetObjectTorrent BucketName
gotrqBucket = lens _gotrqBucket (\ s a -> s{_gotrqBucket = a});

-- | FIXME: Undocumented member.
gotrqKey :: Lens' GetObjectTorrent ObjectKey
gotrqKey = lens _gotrqKey (\ s a -> s{_gotrqKey = a});

instance AWSRequest GetObjectTorrent where
        type Sv GetObjectTorrent = S3
        type Rs GetObjectTorrent = GetObjectTorrentResponse
        request = get
        response
          = receiveBody
              (\ s h x ->
                 GetObjectTorrentResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s))
                     <*> (pure x))

instance ToHeaders GetObjectTorrent where
        toHeaders GetObjectTorrent'{..}
          = mconcat
              ["x-amz-request-payer" =# _gotrqRequestPayer]

instance ToPath GetObjectTorrent where
        toPath GetObjectTorrent'{..}
          = mconcat
              ["/", toText _gotrqBucket, "/", toText _gotrqKey]

instance ToQuery GetObjectTorrent where
        toQuery = const (mconcat ["torrent"])

-- | /See:/ 'getObjectTorrentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotrsRequestCharged'
--
-- * 'gotrsStatus'
--
-- * 'gotrsBody'
data GetObjectTorrentResponse = GetObjectTorrentResponse'
    { _gotrsRequestCharged :: !(Maybe RequestCharged)
    , _gotrsStatus         :: !Int
    , _gotrsBody           :: !RsBody
    } deriving (Show,Generic)

-- | 'GetObjectTorrentResponse' smart constructor.
getObjectTorrentResponse :: Int -> RsBody -> GetObjectTorrentResponse
getObjectTorrentResponse pStatus pBody =
    GetObjectTorrentResponse'
    { _gotrsRequestCharged = Nothing
    , _gotrsStatus = pStatus
    , _gotrsBody = pBody
    }

-- | FIXME: Undocumented member.
gotrsRequestCharged :: Lens' GetObjectTorrentResponse (Maybe RequestCharged)
gotrsRequestCharged = lens _gotrsRequestCharged (\ s a -> s{_gotrsRequestCharged = a});

-- | FIXME: Undocumented member.
gotrsStatus :: Lens' GetObjectTorrentResponse Int
gotrsStatus = lens _gotrsStatus (\ s a -> s{_gotrsStatus = a});

-- | FIXME: Undocumented member.
gotrsBody :: Lens' GetObjectTorrentResponse RsBody
gotrsBody = lens _gotrsBody (\ s a -> s{_gotrsBody = a});
