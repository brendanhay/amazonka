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
-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns torrent files from a bucket. BitTorrent can save you bandwidth when you're distributing large files. For more information about BitTorrent, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3Torrent.html Using BitTorrent with Amazon S3> .
--
--
-- To use GET, you must have READ access to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- The following operation is related to @GetObjectTorrent@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.GetObjectTorrent
  ( -- * Creating a Request
    getObjectTorrent,
    GetObjectTorrent,

    -- * Request Lenses
    gotRequestPayer,
    gotExpectedBucketOwner,
    gotBucket,
    gotKey,

    -- * Destructuring the Response
    getObjectTorrentResponse,
    GetObjectTorrentResponse,

    -- * Response Lenses
    getrsRequestCharged,
    getrsResponseStatus,
    getrsBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getObjectTorrent' smart constructor.
data GetObjectTorrent = GetObjectTorrent'
  { _gotRequestPayer ::
      !(Maybe RequestPayer),
    _gotExpectedBucketOwner :: !(Maybe Text),
    _gotBucket :: !BucketName,
    _gotKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectTorrent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotRequestPayer' - Undocumented member.
--
-- * 'gotExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gotBucket' - The name of the bucket containing the object for which to get the torrent files.
--
-- * 'gotKey' - The object key for which to get the information.
getObjectTorrent ::
  -- | 'gotBucket'
  BucketName ->
  -- | 'gotKey'
  ObjectKey ->
  GetObjectTorrent
getObjectTorrent pBucket_ pKey_ =
  GetObjectTorrent'
    { _gotRequestPayer = Nothing,
      _gotExpectedBucketOwner = Nothing,
      _gotBucket = pBucket_,
      _gotKey = pKey_
    }

-- | Undocumented member.
gotRequestPayer :: Lens' GetObjectTorrent (Maybe RequestPayer)
gotRequestPayer = lens _gotRequestPayer (\s a -> s {_gotRequestPayer = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gotExpectedBucketOwner :: Lens' GetObjectTorrent (Maybe Text)
gotExpectedBucketOwner = lens _gotExpectedBucketOwner (\s a -> s {_gotExpectedBucketOwner = a})

-- | The name of the bucket containing the object for which to get the torrent files.
gotBucket :: Lens' GetObjectTorrent BucketName
gotBucket = lens _gotBucket (\s a -> s {_gotBucket = a})

-- | The object key for which to get the information.
gotKey :: Lens' GetObjectTorrent ObjectKey
gotKey = lens _gotKey (\s a -> s {_gotKey = a})

instance AWSRequest GetObjectTorrent where
  type Rs GetObjectTorrent = GetObjectTorrentResponse
  request = get s3
  response =
    receiveBody
      ( \s h x ->
          GetObjectTorrentResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (pure (fromEnum s))
            <*> (pure x)
      )

instance Hashable GetObjectTorrent

instance NFData GetObjectTorrent

instance ToHeaders GetObjectTorrent where
  toHeaders GetObjectTorrent' {..} =
    mconcat
      [ "x-amz-request-payer" =# _gotRequestPayer,
        "x-amz-expected-bucket-owner" =# _gotExpectedBucketOwner
      ]

instance ToPath GetObjectTorrent where
  toPath GetObjectTorrent' {..} =
    mconcat ["/", toBS _gotBucket, "/", toBS _gotKey]

instance ToQuery GetObjectTorrent where
  toQuery = const (mconcat ["torrent"])

-- | /See:/ 'getObjectTorrentResponse' smart constructor.
data GetObjectTorrentResponse = GetObjectTorrentResponse'
  { _getrsRequestCharged ::
      !(Maybe RequestCharged),
    _getrsResponseStatus :: !Int,
    _getrsBody :: !RsBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'GetObjectTorrentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsRequestCharged' - Undocumented member.
--
-- * 'getrsResponseStatus' - -- | The response status code.
--
-- * 'getrsBody' - A Bencoded dictionary as defined by the BitTorrent specification
getObjectTorrentResponse ::
  -- | 'getrsResponseStatus'
  Int ->
  -- | 'getrsBody'
  RsBody ->
  GetObjectTorrentResponse
getObjectTorrentResponse pResponseStatus_ pBody_ =
  GetObjectTorrentResponse'
    { _getrsRequestCharged = Nothing,
      _getrsResponseStatus = pResponseStatus_,
      _getrsBody = pBody_
    }

-- | Undocumented member.
getrsRequestCharged :: Lens' GetObjectTorrentResponse (Maybe RequestCharged)
getrsRequestCharged = lens _getrsRequestCharged (\s a -> s {_getrsRequestCharged = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetObjectTorrentResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\s a -> s {_getrsResponseStatus = a})

-- | A Bencoded dictionary as defined by the BitTorrent specification
getrsBody :: Lens' GetObjectTorrentResponse RsBody
getrsBody = lens _getrsBody (\s a -> s {_getrsBody = a})
