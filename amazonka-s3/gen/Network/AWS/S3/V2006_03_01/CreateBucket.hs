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
    -- ** Request constructor
    , createBucket
    -- ** Request lenses
    , cbrBucket
    , cbrCreateBucketConfiguration
    , cbrACL
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

-- | Minimum specification for a 'CreateBucket' request.
createBucket :: BucketName -- ^ 'cbrBucket'
             -> CreateBucket
createBucket p1 = CreateBucket
    { _cbrBucket = p1
    , _cbrCreateBucketConfiguration = Nothing
    , _cbrACL = Nothing
    , _cbrGrantFullControl = Nothing
    , _cbrGrantRead = Nothing
    , _cbrGrantReadACP = Nothing
    , _cbrGrantWrite = Nothing
    , _cbrGrantWriteACP = Nothing
    }

data CreateBucket = CreateBucket
    { _cbrBucket :: BucketName
    , _cbrCreateBucketConfiguration :: Maybe CreateBucketConfiguration
    , _cbrACL :: Maybe BucketCannedACL
      -- ^ The canned ACL to apply to the bucket.
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

cbrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> CreateBucket
    -> f CreateBucket
cbrBucket f x =
    (\y -> x { _cbrBucket = y })
       <$> f (_cbrBucket x)
{-# INLINE cbrBucket #-}

cbrCreateBucketConfiguration
    :: Functor f
    => (Maybe CreateBucketConfiguration
    -> f (Maybe CreateBucketConfiguration))
    -> CreateBucket
    -> f CreateBucket
cbrCreateBucketConfiguration f x =
    (\y -> x { _cbrCreateBucketConfiguration = y })
       <$> f (_cbrCreateBucketConfiguration x)
{-# INLINE cbrCreateBucketConfiguration #-}

-- | The canned ACL to apply to the bucket.
cbrACL
    :: Functor f
    => (Maybe BucketCannedACL
    -> f (Maybe BucketCannedACL))
    -> CreateBucket
    -> f CreateBucket
cbrACL f x =
    (\y -> x { _cbrACL = y })
       <$> f (_cbrACL x)
{-# INLINE cbrACL #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the
-- bucket.
cbrGrantFullControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateBucket
    -> f CreateBucket
cbrGrantFullControl f x =
    (\y -> x { _cbrGrantFullControl = y })
       <$> f (_cbrGrantFullControl x)
{-# INLINE cbrGrantFullControl #-}

-- | Allows grantee to list the objects in the bucket.
cbrGrantRead
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateBucket
    -> f CreateBucket
cbrGrantRead f x =
    (\y -> x { _cbrGrantRead = y })
       <$> f (_cbrGrantRead x)
{-# INLINE cbrGrantRead #-}

-- | Allows grantee to read the bucket ACL.
cbrGrantReadACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateBucket
    -> f CreateBucket
cbrGrantReadACP f x =
    (\y -> x { _cbrGrantReadACP = y })
       <$> f (_cbrGrantReadACP x)
{-# INLINE cbrGrantReadACP #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
cbrGrantWrite
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateBucket
    -> f CreateBucket
cbrGrantWrite f x =
    (\y -> x { _cbrGrantWrite = y })
       <$> f (_cbrGrantWrite x)
{-# INLINE cbrGrantWrite #-}

-- | Allows grantee to write the ACL for the applicable bucket.
cbrGrantWriteACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateBucket
    -> f CreateBucket
cbrGrantWriteACP f x =
    (\y -> x { _cbrGrantWriteACP = y })
       <$> f (_cbrGrantWriteACP x)
{-# INLINE cbrGrantWriteACP #-}

instance ToPath CreateBucket where
    toPath CreateBucket{..} = mconcat
        [ "/"
        , toBS _cbrBucket
        ]

instance ToQuery CreateBucket

instance ToHeaders CreateBucket where
    toHeaders CreateBucket{..} = concat
        [ "x-amz-acl" =: _cbrACL
        , "x-amz-grant-full-control" =: _cbrGrantFullControl
        , "x-amz-grant-read" =: _cbrGrantRead
        , "x-amz-grant-read-acp" =: _cbrGrantReadACP
        , "x-amz-grant-write" =: _cbrGrantWrite
        , "x-amz-grant-write-acp" =: _cbrGrantWriteACP
        ]

instance ToBody CreateBucket where
    toBody = toBody . encodeXML . _cbrCreateBucketConfiguration

data CreateBucketResponse = CreateBucketResponse
    { _cboLocation :: Maybe Text
    } deriving (Show, Generic)

cboLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateBucketResponse
    -> f CreateBucketResponse
cboLocation f x =
    (\y -> x { _cboLocation = y })
       <$> f (_cboLocation x)
{-# INLINE cboLocation #-}

instance AWSRequest CreateBucket where
    type Sv CreateBucket = S3
    type Rs CreateBucket = CreateBucketResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure CreateBucketResponse
            <*> hs ~:? "Location"
