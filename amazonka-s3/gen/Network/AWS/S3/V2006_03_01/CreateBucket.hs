{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CreateBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new bucket.
module Network.AWS.S3.V2006_03_01.CreateBucket
    (
    -- * Request
      CreateBucket
    -- ** Request alias
    , PutBucket
    -- ** Request constructor
    , mkCreateBucketRequest
    -- ** Request lenses
    , cbrACL
    , cbrBucket
    , cbrCreateBucketConfiguration
    , cbrGrantFullControl
    , cbrGrantRead
    , cbrGrantReadACP
    , cbrGrantWrite
    , cbrGrantWriteACP

    -- * Response
    , CreateBucketResponse
    -- ** Response lenses
    , cboLocation
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type PutBucket = CreateBucket

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateBucket' request.
mkCreateBucketRequest :: BucketName -- ^ 'cbrBucket'
                      -> CreateBucket
mkCreateBucketRequest p1 = CreateBucket
    { _cbrACL = Nothing
    , _cbrBucket = p2
    , _cbrCreateBucketConfiguration = Nothing
    , _cbrGrantFullControl = Nothing
    , _cbrGrantRead = Nothing
    , _cbrGrantReadACP = Nothing
    , _cbrGrantWrite = Nothing
    , _cbrGrantWriteACP = Nothing
    }
{-# INLINE mkCreateBucketRequest #-}

data CreateBucket = CreateBucket
    { _cbrACL :: Maybe BucketCannedACL
      -- ^ The canned ACL to apply to the bucket.
    , _cbrBucket :: BucketName
    , _cbrCreateBucketConfiguration :: Maybe CreateBucketConfiguration
    , _cbrGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , _cbrGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , _cbrGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , _cbrGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , _cbrGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Show, Generic)

-- | The canned ACL to apply to the bucket.
cbrACL :: Lens' CreateBucket (Maybe BucketCannedACL)
cbrACL = lens _cbrACL (\s a -> s { _cbrACL = a })
{-# INLINE cbrACL #-}

cbrBucket :: Lens' CreateBucket (BucketName)
cbrBucket = lens _cbrBucket (\s a -> s { _cbrBucket = a })
{-# INLINE cbrBucket #-}

cbrCreateBucketConfiguration :: Lens' CreateBucket (Maybe CreateBucketConfiguration)
cbrCreateBucketConfiguration = lens _cbrCreateBucketConfiguration (\s a -> s { _cbrCreateBucketConfiguration = a })
{-# INLINE cbrCreateBucketConfiguration #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the
-- bucket.
cbrGrantFullControl :: Lens' CreateBucket (Maybe Text)
cbrGrantFullControl = lens _cbrGrantFullControl (\s a -> s { _cbrGrantFullControl = a })
{-# INLINE cbrGrantFullControl #-}

-- | Allows grantee to list the objects in the bucket.
cbrGrantRead :: Lens' CreateBucket (Maybe Text)
cbrGrantRead = lens _cbrGrantRead (\s a -> s { _cbrGrantRead = a })
{-# INLINE cbrGrantRead #-}

-- | Allows grantee to read the bucket ACL.
cbrGrantReadACP :: Lens' CreateBucket (Maybe Text)
cbrGrantReadACP = lens _cbrGrantReadACP (\s a -> s { _cbrGrantReadACP = a })
{-# INLINE cbrGrantReadACP #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
cbrGrantWrite :: Lens' CreateBucket (Maybe Text)
cbrGrantWrite = lens _cbrGrantWrite (\s a -> s { _cbrGrantWrite = a })
{-# INLINE cbrGrantWrite #-}

-- | Allows grantee to write the ACL for the applicable bucket.
cbrGrantWriteACP :: Lens' CreateBucket (Maybe Text)
cbrGrantWriteACP = lens _cbrGrantWriteACP (\s a -> s { _cbrGrantWriteACP = a })
{-# INLINE cbrGrantWriteACP #-}

instance ToPath CreateBucket where
    toPath CreateBucket{..} = mconcat
        [ "/"
        , toBS _cbrBucket
        ]

instance ToQuery CreateBucket

instance ToHeaders CreateBucket

instance ToBody CreateBucket

newtype CreateBucketResponse = CreateBucketResponse
    { _cboLocation :: Maybe Text
    } deriving (Show, Generic)

cboLocation :: Lens' CreateBucketResponse (Maybe Text)
cboLocation = lens _cboLocation (\s a -> s { _cboLocation = a })
{-# INLINE cboLocation #-}

instance AWSRequest CreateBucket where
    type Sv CreateBucket = S3
    type Rs CreateBucket = CreateBucketResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure CreateBucketResponse
            <*> hs ~:? "Location"
