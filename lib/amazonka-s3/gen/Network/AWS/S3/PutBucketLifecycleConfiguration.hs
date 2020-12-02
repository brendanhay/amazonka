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
-- Module      : Network.AWS.S3.PutBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new lifecycle configuration for the bucket or replaces an existing lifecycle configuration. For information about lifecycle configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
--
-- __Rules__
--
-- You specify the lifecycle configuration in your request body. The lifecycle configuration is specified as XML consisting of one or more rules. Each rule consists of the following:
--
--     * Filter identifying a subset of objects to which the rule applies. The filter can be based on a key name prefix, object tags, or a combination of both.
--
--     * Status whether the rule is in effect.
--
--     * One or more lifecycle transition and expiration actions that you want Amazon S3 to perform on the objects identified by the filter. If the state of your bucket is versioning-enabled or versioning-suspended, you can have many versions of the same object (one current version and zero or more noncurrent versions). Amazon S3 provides predefined actions that you can specify for current and noncurrent object versions.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html Lifecycle Configuration Elements> .
--
-- __Permissions__
--
-- By default, all Amazon S3 resources are private, including buckets, objects, and related subresources (for example, lifecycle configuration and website configuration). Only the resource owner (that is, the AWS account that created it) can access the resource. The resource owner can optionally grant access permissions to others by writing an access policy. For this operation, a user must get the s3:PutLifecycleConfiguration permission.
--
-- You can also explicitly deny permissions. Explicit deny also supersedes any other permissions. If you want to block users or accounts from removing or deleting objects from your bucket, you must deny them permissions for the following actions:
--
--     * s3:DeleteObject
--
--     * s3:DeleteObjectVersion
--
--     * s3:PutLifecycleConfiguration
--
--
--
-- For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- The following are related to @PutBucketLifecycleConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-configuration-examples.html Examples of Lifecycle Configuration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketLifecycle.html DeleteBucketLifecycle>
module Network.AWS.S3.PutBucketLifecycleConfiguration
  ( -- * Creating a Request
    putBucketLifecycleConfiguration,
    PutBucketLifecycleConfiguration,

    -- * Request Lenses
    pblcLifecycleConfiguration,
    pblcExpectedBucketOwner,
    pblcBucket,

    -- * Destructuring the Response
    putBucketLifecycleConfigurationResponse,
    PutBucketLifecycleConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketLifecycleConfiguration' smart constructor.
data PutBucketLifecycleConfiguration = PutBucketLifecycleConfiguration'
  { _pblcLifecycleConfiguration ::
      !( Maybe
           BucketLifecycleConfiguration
       ),
    _pblcExpectedBucketOwner ::
      !(Maybe Text),
    _pblcBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pblcLifecycleConfiguration' - Container for lifecycle rules. You can add as many as 1,000 rules.
--
-- * 'pblcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pblcBucket' - The name of the bucket for which to set the configuration.
putBucketLifecycleConfiguration ::
  -- | 'pblcBucket'
  BucketName ->
  PutBucketLifecycleConfiguration
putBucketLifecycleConfiguration pBucket_ =
  PutBucketLifecycleConfiguration'
    { _pblcLifecycleConfiguration =
        Nothing,
      _pblcExpectedBucketOwner = Nothing,
      _pblcBucket = pBucket_
    }

-- | Container for lifecycle rules. You can add as many as 1,000 rules.
pblcLifecycleConfiguration :: Lens' PutBucketLifecycleConfiguration (Maybe BucketLifecycleConfiguration)
pblcLifecycleConfiguration = lens _pblcLifecycleConfiguration (\s a -> s {_pblcLifecycleConfiguration = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pblcExpectedBucketOwner :: Lens' PutBucketLifecycleConfiguration (Maybe Text)
pblcExpectedBucketOwner = lens _pblcExpectedBucketOwner (\s a -> s {_pblcExpectedBucketOwner = a})

-- | The name of the bucket for which to set the configuration.
pblcBucket :: Lens' PutBucketLifecycleConfiguration BucketName
pblcBucket = lens _pblcBucket (\s a -> s {_pblcBucket = a})

instance AWSRequest PutBucketLifecycleConfiguration where
  type
    Rs PutBucketLifecycleConfiguration =
      PutBucketLifecycleConfigurationResponse
  request = putXML s3
  response = receiveNull PutBucketLifecycleConfigurationResponse'

instance Hashable PutBucketLifecycleConfiguration

instance NFData PutBucketLifecycleConfiguration

instance ToElement PutBucketLifecycleConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
      . _pblcLifecycleConfiguration

instance ToHeaders PutBucketLifecycleConfiguration where
  toHeaders PutBucketLifecycleConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _pblcExpectedBucketOwner]

instance ToPath PutBucketLifecycleConfiguration where
  toPath PutBucketLifecycleConfiguration' {..} =
    mconcat ["/", toBS _pblcBucket]

instance ToQuery PutBucketLifecycleConfiguration where
  toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'putBucketLifecycleConfigurationResponse' smart constructor.
data PutBucketLifecycleConfigurationResponse = PutBucketLifecycleConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketLifecycleConfigurationResponse' with the minimum fields required to make a request.
putBucketLifecycleConfigurationResponse ::
  PutBucketLifecycleConfigurationResponse
putBucketLifecycleConfigurationResponse =
  PutBucketLifecycleConfigurationResponse'

instance NFData PutBucketLifecycleConfigurationResponse
