{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the permissions on a bucket using access control lists (ACL).
module Network.AWS.S3.V2006_03_01.PutBucketAcl
    (
    -- * Request
      PutBucketAcl
    -- ** Request constructor
    , mkPutBucketAclRequest
    -- ** Request lenses
    , pbarACL
    , pbarAccessControlPolicy
    , pbarBucket
    , pbarContentMD5
    , pbarGrantFullControl
    , pbarGrantRead
    , pbarGrantReadACP
    , pbarGrantWrite
    , pbarGrantWriteACP

    -- * Response
    , PutBucketAclResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketAcl' request.
mkPutBucketAclRequest :: BucketName -- ^ 'pbarBucket'
                      -> PutBucketAcl
mkPutBucketAclRequest p1 = PutBucketAcl
    { _pbarACL = Nothing
    , _pbarAccessControlPolicy = Nothing
    , _pbarBucket = p3
    , _pbarContentMD5 = Nothing
    , _pbarGrantFullControl = Nothing
    , _pbarGrantRead = Nothing
    , _pbarGrantReadACP = Nothing
    , _pbarGrantWrite = Nothing
    , _pbarGrantWriteACP = Nothing
    }
{-# INLINE mkPutBucketAclRequest #-}

data PutBucketAcl = PutBucketAcl
    { _pbarACL :: Maybe BucketCannedACL
      -- ^ The canned ACL to apply to the bucket.
    , _pbarAccessControlPolicy :: Maybe AccessControlPolicy
    , _pbarBucket :: BucketName
    , _pbarContentMD5 :: Maybe Text
    , _pbarGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , _pbarGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , _pbarGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , _pbarGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , _pbarGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Show, Generic)

-- | The canned ACL to apply to the bucket.
pbarACL :: Lens' PutBucketAcl (Maybe BucketCannedACL)
pbarACL = lens _pbarACL (\s a -> s { _pbarACL = a })
{-# INLINE pbarACL #-}

pbarAccessControlPolicy :: Lens' PutBucketAcl (Maybe AccessControlPolicy)
pbarAccessControlPolicy = lens _pbarAccessControlPolicy (\s a -> s { _pbarAccessControlPolicy = a })
{-# INLINE pbarAccessControlPolicy #-}

pbarBucket :: Lens' PutBucketAcl (BucketName)
pbarBucket = lens _pbarBucket (\s a -> s { _pbarBucket = a })
{-# INLINE pbarBucket #-}

pbarContentMD5 :: Lens' PutBucketAcl (Maybe Text)
pbarContentMD5 = lens _pbarContentMD5 (\s a -> s { _pbarContentMD5 = a })
{-# INLINE pbarContentMD5 #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the
-- bucket.
pbarGrantFullControl :: Lens' PutBucketAcl (Maybe Text)
pbarGrantFullControl = lens _pbarGrantFullControl (\s a -> s { _pbarGrantFullControl = a })
{-# INLINE pbarGrantFullControl #-}

-- | Allows grantee to list the objects in the bucket.
pbarGrantRead :: Lens' PutBucketAcl (Maybe Text)
pbarGrantRead = lens _pbarGrantRead (\s a -> s { _pbarGrantRead = a })
{-# INLINE pbarGrantRead #-}

-- | Allows grantee to read the bucket ACL.
pbarGrantReadACP :: Lens' PutBucketAcl (Maybe Text)
pbarGrantReadACP = lens _pbarGrantReadACP (\s a -> s { _pbarGrantReadACP = a })
{-# INLINE pbarGrantReadACP #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
pbarGrantWrite :: Lens' PutBucketAcl (Maybe Text)
pbarGrantWrite = lens _pbarGrantWrite (\s a -> s { _pbarGrantWrite = a })
{-# INLINE pbarGrantWrite #-}

-- | Allows grantee to write the ACL for the applicable bucket.
pbarGrantWriteACP :: Lens' PutBucketAcl (Maybe Text)
pbarGrantWriteACP = lens _pbarGrantWriteACP (\s a -> s { _pbarGrantWriteACP = a })
{-# INLINE pbarGrantWriteACP #-}

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = mconcat
        [ "/"
        , toBS _pbarBucket
        ]

instance ToQuery PutBucketAcl where
    toQuery PutBucketAcl{..} = mconcat
        [ "acl"
        ]

instance ToHeaders PutBucketAcl

instance ToBody PutBucketAcl

data PutBucketAclResponse = PutBucketAclResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketAcl where
    type Sv PutBucketAcl = S3
    type Rs PutBucketAcl = PutBucketAclResponse

    request = put
    response _ = nullaryResponse PutBucketAclResponse
