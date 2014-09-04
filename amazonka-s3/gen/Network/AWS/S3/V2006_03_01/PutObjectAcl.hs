{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | uses the acl subresource to set the access control list (ACL) permissions
-- for an object that already exists in a bucket.
module Network.AWS.S3.V2006_03_01.PutObjectAcl
    (
    -- * Request
      PutObjectAcl
    -- ** Request constructor
    , mkPutObjectAclRequest
    -- ** Request lenses
    , poarACL
    , poarAccessControlPolicy
    , poarBucket
    , poarContentMD5
    , poarGrantFullControl
    , poarGrantRead
    , poarGrantReadACP
    , poarGrantWrite
    , poarGrantWriteACP
    , poarKey

    -- * Response
    , PutObjectAclResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutObjectAcl' request.
mkPutObjectAclRequest :: BucketName -- ^ 'poarBucket'
                      -> ObjectKey -- ^ 'poarKey'
                      -> PutObjectAcl
mkPutObjectAclRequest p1 p2 = PutObjectAcl
    { _poarACL = Nothing
    , _poarAccessControlPolicy = Nothing
    , _poarBucket = p3
    , _poarContentMD5 = Nothing
    , _poarGrantFullControl = Nothing
    , _poarGrantRead = Nothing
    , _poarGrantReadACP = Nothing
    , _poarGrantWrite = Nothing
    , _poarGrantWriteACP = Nothing
    , _poarKey = p10
    }
{-# INLINE mkPutObjectAclRequest #-}

data PutObjectAcl = PutObjectAcl
    { _poarACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _poarAccessControlPolicy :: Maybe AccessControlPolicy
    , _poarBucket :: BucketName
    , _poarContentMD5 :: Maybe Text
    , _poarGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , _poarGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , _poarGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , _poarGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , _poarGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    , _poarKey :: ObjectKey
    } deriving (Show, Generic)

-- | The canned ACL to apply to the object.
poarACL :: Lens' PutObjectAcl (Maybe ObjectCannedACL)
poarACL = lens _poarACL (\s a -> s { _poarACL = a })
{-# INLINE poarACL #-}

poarAccessControlPolicy :: Lens' PutObjectAcl (Maybe AccessControlPolicy)
poarAccessControlPolicy = lens _poarAccessControlPolicy (\s a -> s { _poarAccessControlPolicy = a })
{-# INLINE poarAccessControlPolicy #-}

poarBucket :: Lens' PutObjectAcl (BucketName)
poarBucket = lens _poarBucket (\s a -> s { _poarBucket = a })
{-# INLINE poarBucket #-}

poarContentMD5 :: Lens' PutObjectAcl (Maybe Text)
poarContentMD5 = lens _poarContentMD5 (\s a -> s { _poarContentMD5 = a })
{-# INLINE poarContentMD5 #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the
-- bucket.
poarGrantFullControl :: Lens' PutObjectAcl (Maybe Text)
poarGrantFullControl = lens _poarGrantFullControl (\s a -> s { _poarGrantFullControl = a })
{-# INLINE poarGrantFullControl #-}

-- | Allows grantee to list the objects in the bucket.
poarGrantRead :: Lens' PutObjectAcl (Maybe Text)
poarGrantRead = lens _poarGrantRead (\s a -> s { _poarGrantRead = a })
{-# INLINE poarGrantRead #-}

-- | Allows grantee to read the bucket ACL.
poarGrantReadACP :: Lens' PutObjectAcl (Maybe Text)
poarGrantReadACP = lens _poarGrantReadACP (\s a -> s { _poarGrantReadACP = a })
{-# INLINE poarGrantReadACP #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
poarGrantWrite :: Lens' PutObjectAcl (Maybe Text)
poarGrantWrite = lens _poarGrantWrite (\s a -> s { _poarGrantWrite = a })
{-# INLINE poarGrantWrite #-}

-- | Allows grantee to write the ACL for the applicable bucket.
poarGrantWriteACP :: Lens' PutObjectAcl (Maybe Text)
poarGrantWriteACP = lens _poarGrantWriteACP (\s a -> s { _poarGrantWriteACP = a })
{-# INLINE poarGrantWriteACP #-}

poarKey :: Lens' PutObjectAcl (ObjectKey)
poarKey = lens _poarKey (\s a -> s { _poarKey = a })
{-# INLINE poarKey #-}

instance ToPath PutObjectAcl where
    toPath PutObjectAcl{..} = mconcat
        [ "/"
        , toBS _poarBucket
        , "/"
        , toBS _poarKey
        ]

instance ToQuery PutObjectAcl where
    toQuery PutObjectAcl{..} = mconcat
        [ "acl"
        ]

instance ToHeaders PutObjectAcl

instance ToBody PutObjectAcl

data PutObjectAclResponse = PutObjectAclResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutObjectAcl where
    type Sv PutObjectAcl = S3
    type Rs PutObjectAcl = PutObjectAclResponse

    request = put
    response _ = nullaryResponse PutObjectAclResponse
