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
-- Module      : Network.AWS.S3.PutBucketLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. All logs are saved to buckets in the same AWS Region as the source bucket. To set the logging status of a bucket, you must be the bucket owner.
--
--
-- The bucket owner is automatically granted FULL_CONTROL to all logs. You use the @Grantee@ request element to grant access to other people. The @Permissions@ request element specifies the kind of access the grantee has to the logs.
--
-- __Grantee Values__
--
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
--
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@
--
-- DisplayName is optional and ignored in the request.
--
--     * By Email address:
--
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail"><EmailAddress><>Grantees@email.com<></EmailAddress></Grantee>@
--
-- The grantee is resolved to the CanonicalUser and, in a response to a GET Object acl request, appears as the CanonicalUser.
--
--     * By URI:
--
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group"><URI><>http://acs.amazonaws.com/groups/global/AuthenticatedUsers<></URI></Grantee>@
--
--
--
-- To enable logging, you use LoggingEnabled and its children request elements. To disable logging, you use an empty BucketLoggingStatus request element:
--
-- @<BucketLoggingStatus xmlns="http://doc.s3.amazonaws.com/2006-03-01" />@
--
-- For more information about server access logging, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerLogs.html Server Access Logging> .
--
-- For more information about creating a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> . For more information about returning the logging status of a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLogging.html GetBucketLogging> .
--
-- The following operations are related to @PutBucketLogging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLogging.html GetBucketLogging>
module Network.AWS.S3.PutBucketLogging
  ( -- * Creating a Request
    putBucketLogging,
    PutBucketLogging,

    -- * Request Lenses
    pblContentMD5,
    pblExpectedBucketOwner,
    pblBucket,
    pblBucketLoggingStatus,

    -- * Destructuring the Response
    putBucketLoggingResponse,
    PutBucketLoggingResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketLogging' smart constructor.
data PutBucketLogging = PutBucketLogging'
  { _pblContentMD5 ::
      !(Maybe Text),
    _pblExpectedBucketOwner :: !(Maybe Text),
    _pblBucket :: !BucketName,
    _pblBucketLoggingStatus :: !BucketLoggingStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pblContentMD5' - The MD5 hash of the @PutBucketLogging@ request body. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pblExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pblBucket' - The name of the bucket for which to set the logging parameters.
--
-- * 'pblBucketLoggingStatus' - Container for logging status information.
putBucketLogging ::
  -- | 'pblBucket'
  BucketName ->
  -- | 'pblBucketLoggingStatus'
  BucketLoggingStatus ->
  PutBucketLogging
putBucketLogging pBucket_ pBucketLoggingStatus_ =
  PutBucketLogging'
    { _pblContentMD5 = Nothing,
      _pblExpectedBucketOwner = Nothing,
      _pblBucket = pBucket_,
      _pblBucketLoggingStatus = pBucketLoggingStatus_
    }

-- | The MD5 hash of the @PutBucketLogging@ request body. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pblContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblContentMD5 = lens _pblContentMD5 (\s a -> s {_pblContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pblExpectedBucketOwner :: Lens' PutBucketLogging (Maybe Text)
pblExpectedBucketOwner = lens _pblExpectedBucketOwner (\s a -> s {_pblExpectedBucketOwner = a})

-- | The name of the bucket for which to set the logging parameters.
pblBucket :: Lens' PutBucketLogging BucketName
pblBucket = lens _pblBucket (\s a -> s {_pblBucket = a})

-- | Container for logging status information.
pblBucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pblBucketLoggingStatus = lens _pblBucketLoggingStatus (\s a -> s {_pblBucketLoggingStatus = a})

instance AWSRequest PutBucketLogging where
  type Rs PutBucketLogging = PutBucketLoggingResponse
  request = putXML s3
  response = receiveNull PutBucketLoggingResponse'

instance Hashable PutBucketLogging

instance NFData PutBucketLogging

instance ToElement PutBucketLogging where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}BucketLoggingStatus"
      . _pblBucketLoggingStatus

instance ToHeaders PutBucketLogging where
  toHeaders PutBucketLogging' {..} =
    mconcat
      [ "Content-MD5" =# _pblContentMD5,
        "x-amz-expected-bucket-owner" =# _pblExpectedBucketOwner
      ]

instance ToPath PutBucketLogging where
  toPath PutBucketLogging' {..} = mconcat ["/", toBS _pblBucket]

instance ToQuery PutBucketLogging where
  toQuery = const (mconcat ["logging"])

-- | /See:/ 'putBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse = PutBucketLoggingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketLoggingResponse' with the minimum fields required to make a request.
putBucketLoggingResponse ::
  PutBucketLoggingResponse
putBucketLoggingResponse = PutBucketLoggingResponse'

instance NFData PutBucketLoggingResponse
