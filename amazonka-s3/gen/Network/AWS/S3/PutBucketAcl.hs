{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the permissions on a bucket using access control lists (ACL).
module Network.AWS.S3.PutBucketAcl
    (
    -- * Request
      PutBucketAcl
    -- ** Request constructor
    , putBucketAcl
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
    -- ** Response constructor
    , putBucketAclResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutBucketAcl = PutBucketAcl
    { _pbarACL                 :: Maybe Text
    , _pbarAccessControlPolicy :: Maybe AccessControlPolicy
    , _pbarBucket              :: Text
    , _pbarContentMD5          :: Maybe Text
    , _pbarGrantFullControl    :: Maybe Text
    , _pbarGrantRead           :: Maybe Text
    , _pbarGrantReadACP        :: Maybe Text
    , _pbarGrantWrite          :: Maybe Text
    , _pbarGrantWriteACP       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PutBucketAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbarACL' @::@ 'Maybe' 'Text'
--
-- * 'pbarAccessControlPolicy' @::@ 'Maybe' 'AccessControlPolicy'
--
-- * 'pbarBucket' @::@ 'Text'
--
-- * 'pbarContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbarGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'pbarGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'pbarGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'pbarGrantWrite' @::@ 'Maybe' 'Text'
--
-- * 'pbarGrantWriteACP' @::@ 'Maybe' 'Text'
--
putBucketAcl :: Text -- ^ 'pbarBucket'
             -> PutBucketAcl
putBucketAcl p1 = PutBucketAcl
    { _pbarBucket              = p1
    , _pbarACL                 = Nothing
    , _pbarAccessControlPolicy = Nothing
    , _pbarContentMD5          = Nothing
    , _pbarGrantFullControl    = Nothing
    , _pbarGrantRead           = Nothing
    , _pbarGrantReadACP        = Nothing
    , _pbarGrantWrite          = Nothing
    , _pbarGrantWriteACP       = Nothing
    }

-- | The canned ACL to apply to the bucket.
pbarACL :: Lens' PutBucketAcl (Maybe Text)
pbarACL = lens _pbarACL (\s a -> s { _pbarACL = a })

pbarAccessControlPolicy :: Lens' PutBucketAcl (Maybe AccessControlPolicy)
pbarAccessControlPolicy =
    lens _pbarAccessControlPolicy (\s a -> s { _pbarAccessControlPolicy = a })

pbarBucket :: Lens' PutBucketAcl Text
pbarBucket = lens _pbarBucket (\s a -> s { _pbarBucket = a })

pbarContentMD5 :: Lens' PutBucketAcl (Maybe Text)
pbarContentMD5 = lens _pbarContentMD5 (\s a -> s { _pbarContentMD5 = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
pbarGrantFullControl :: Lens' PutBucketAcl (Maybe Text)
pbarGrantFullControl =
    lens _pbarGrantFullControl (\s a -> s { _pbarGrantFullControl = a })

-- | Allows grantee to list the objects in the bucket.
pbarGrantRead :: Lens' PutBucketAcl (Maybe Text)
pbarGrantRead = lens _pbarGrantRead (\s a -> s { _pbarGrantRead = a })

-- | Allows grantee to read the bucket ACL.
pbarGrantReadACP :: Lens' PutBucketAcl (Maybe Text)
pbarGrantReadACP = lens _pbarGrantReadACP (\s a -> s { _pbarGrantReadACP = a })

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
pbarGrantWrite :: Lens' PutBucketAcl (Maybe Text)
pbarGrantWrite = lens _pbarGrantWrite (\s a -> s { _pbarGrantWrite = a })

-- | Allows grantee to write the ACL for the applicable bucket.
pbarGrantWriteACP :: Lens' PutBucketAcl (Maybe Text)
pbarGrantWriteACP =
    lens _pbarGrantWriteACP (\s a -> s { _pbarGrantWriteACP = a })

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = mconcat
        [ "/"
        , toText _pbarBucket
        ]

instance ToQuery PutBucketAcl where
    toQuery = const "acl"

instance ToHeaders PutBucketAcl where
    toHeaders PutBucketAcl{..} = mconcat
        [ "x-amz-acl"                =: _pbarACL
        , "Content-MD5"              =: _pbarContentMD5
        , "x-amz-grant-full-control" =: _pbarGrantFullControl
        , "x-amz-grant-read"         =: _pbarGrantRead
        , "x-amz-grant-read-acp"     =: _pbarGrantReadACP
        , "x-amz-grant-write"        =: _pbarGrantWrite
        , "x-amz-grant-write-acp"    =: _pbarGrantWriteACP
        ]

instance ToBody PutBucketAcl where
    toBody = toBody . encodeXML . _pbarAccessControlPolicy

data PutBucketAclResponse = PutBucketAclResponse
-- | 'PutBucketAclResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
putBucketAclResponse :: PutBucketAclResponse
putBucketAclResponse = PutBucketAclResponse

instance AWSRequest PutBucketAcl where
    type Sv PutBucketAcl = S3
    type Rs PutBucketAcl = PutBucketAclResponse

    request  = put'
    response = const (nullaryResponse PutBucketAclResponse)
