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
-- Module      : Network.AWS.S3.PutBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
--
-- The following operations are related to @PutBucketOwnershipControls@ :
--
--     * 'GetBucketOwnershipControls'
--
--     * 'DeleteBucketOwnershipControls'
module Network.AWS.S3.PutBucketOwnershipControls
  ( -- * Creating a Request
    putBucketOwnershipControls,
    PutBucketOwnershipControls,

    -- * Request Lenses
    pbocContentMD5,
    pbocExpectedBucketOwner,
    pbocBucket,
    pbocOwnershipControls,

    -- * Destructuring the Response
    putBucketOwnershipControlsResponse,
    PutBucketOwnershipControlsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketOwnershipControls' smart constructor.
data PutBucketOwnershipControls = PutBucketOwnershipControls'
  { _pbocContentMD5 ::
      !(Maybe Text),
    _pbocExpectedBucketOwner ::
      !(Maybe Text),
    _pbocBucket :: !BucketName,
    _pbocOwnershipControls ::
      !OwnershipControls
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketOwnershipControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbocContentMD5' - The MD5 hash of the @OwnershipControls@ request body.  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbocExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbocBucket' - The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
--
-- * 'pbocOwnershipControls' - The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
putBucketOwnershipControls ::
  -- | 'pbocBucket'
  BucketName ->
  -- | 'pbocOwnershipControls'
  OwnershipControls ->
  PutBucketOwnershipControls
putBucketOwnershipControls pBucket_ pOwnershipControls_ =
  PutBucketOwnershipControls'
    { _pbocContentMD5 = Nothing,
      _pbocExpectedBucketOwner = Nothing,
      _pbocBucket = pBucket_,
      _pbocOwnershipControls = pOwnershipControls_
    }

-- | The MD5 hash of the @OwnershipControls@ request body.  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbocContentMD5 :: Lens' PutBucketOwnershipControls (Maybe Text)
pbocContentMD5 = lens _pbocContentMD5 (\s a -> s {_pbocContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbocExpectedBucketOwner :: Lens' PutBucketOwnershipControls (Maybe Text)
pbocExpectedBucketOwner = lens _pbocExpectedBucketOwner (\s a -> s {_pbocExpectedBucketOwner = a})

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
pbocBucket :: Lens' PutBucketOwnershipControls BucketName
pbocBucket = lens _pbocBucket (\s a -> s {_pbocBucket = a})

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
pbocOwnershipControls :: Lens' PutBucketOwnershipControls OwnershipControls
pbocOwnershipControls = lens _pbocOwnershipControls (\s a -> s {_pbocOwnershipControls = a})

instance AWSRequest PutBucketOwnershipControls where
  type
    Rs PutBucketOwnershipControls =
      PutBucketOwnershipControlsResponse
  request = putXML s3
  response = receiveNull PutBucketOwnershipControlsResponse'

instance Hashable PutBucketOwnershipControls

instance NFData PutBucketOwnershipControls

instance ToElement PutBucketOwnershipControls where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}OwnershipControls"
      . _pbocOwnershipControls

instance ToHeaders PutBucketOwnershipControls where
  toHeaders PutBucketOwnershipControls' {..} =
    mconcat
      [ "Content-MD5" =# _pbocContentMD5,
        "x-amz-expected-bucket-owner" =# _pbocExpectedBucketOwner
      ]

instance ToPath PutBucketOwnershipControls where
  toPath PutBucketOwnershipControls' {..} =
    mconcat ["/", toBS _pbocBucket]

instance ToQuery PutBucketOwnershipControls where
  toQuery = const (mconcat ["ownershipControls"])

-- | /See:/ 'putBucketOwnershipControlsResponse' smart constructor.
data PutBucketOwnershipControlsResponse = PutBucketOwnershipControlsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketOwnershipControlsResponse' with the minimum fields required to make a request.
putBucketOwnershipControlsResponse ::
  PutBucketOwnershipControlsResponse
putBucketOwnershipControlsResponse =
  PutBucketOwnershipControlsResponse'

instance NFData PutBucketOwnershipControlsResponse
