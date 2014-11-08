{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutObjectAcl
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
module Network.AWS.S3.PutObjectAcl
    (
    -- * Request
      PutObjectAcl
    -- ** Request constructor
    , putObjectAcl
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
    -- ** Response constructor
    , putObjectAclResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data PutObjectAcl = PutObjectAcl
    { _poarACL                 :: Maybe Text
    , _poarAccessControlPolicy :: Maybe AccessControlPolicy
    , _poarBucket              :: Text
    , _poarContentMD5          :: Maybe Text
    , _poarGrantFullControl    :: Maybe Text
    , _poarGrantRead           :: Maybe Text
    , _poarGrantReadACP        :: Maybe Text
    , _poarGrantWrite          :: Maybe Text
    , _poarGrantWriteACP       :: Maybe Text
    , _poarKey                 :: Text
    } deriving (Eq, Show, Generic)

-- | 'PutObjectAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poarACL' @::@ 'Maybe' 'Text'
--
-- * 'poarAccessControlPolicy' @::@ 'Maybe' 'AccessControlPolicy'
--
-- * 'poarBucket' @::@ 'Text'
--
-- * 'poarContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'poarGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'poarGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'poarGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'poarGrantWrite' @::@ 'Maybe' 'Text'
--
-- * 'poarGrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'poarKey' @::@ 'Text'
--
putObjectAcl :: Text -- ^ 'poarBucket'
             -> Text -- ^ 'poarKey'
             -> PutObjectAcl
putObjectAcl p1 p2 = PutObjectAcl
    { _poarBucket              = p1
    , _poarKey                 = p2
    , _poarACL                 = Nothing
    , _poarAccessControlPolicy = Nothing
    , _poarContentMD5          = Nothing
    , _poarGrantFullControl    = Nothing
    , _poarGrantRead           = Nothing
    , _poarGrantReadACP        = Nothing
    , _poarGrantWrite          = Nothing
    , _poarGrantWriteACP       = Nothing
    }

-- | The canned ACL to apply to the object.
poarACL :: Lens' PutObjectAcl (Maybe Text)
poarACL = lens _poarACL (\s a -> s { _poarACL = a })

poarAccessControlPolicy :: Lens' PutObjectAcl (Maybe AccessControlPolicy)
poarAccessControlPolicy =
    lens _poarAccessControlPolicy (\s a -> s { _poarAccessControlPolicy = a })

poarBucket :: Lens' PutObjectAcl Text
poarBucket = lens _poarBucket (\s a -> s { _poarBucket = a })

poarContentMD5 :: Lens' PutObjectAcl (Maybe Text)
poarContentMD5 = lens _poarContentMD5 (\s a -> s { _poarContentMD5 = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
poarGrantFullControl :: Lens' PutObjectAcl (Maybe Text)
poarGrantFullControl =
    lens _poarGrantFullControl (\s a -> s { _poarGrantFullControl = a })

-- | Allows grantee to list the objects in the bucket.
poarGrantRead :: Lens' PutObjectAcl (Maybe Text)
poarGrantRead = lens _poarGrantRead (\s a -> s { _poarGrantRead = a })

-- | Allows grantee to read the bucket ACL.
poarGrantReadACP :: Lens' PutObjectAcl (Maybe Text)
poarGrantReadACP = lens _poarGrantReadACP (\s a -> s { _poarGrantReadACP = a })

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
poarGrantWrite :: Lens' PutObjectAcl (Maybe Text)
poarGrantWrite = lens _poarGrantWrite (\s a -> s { _poarGrantWrite = a })

-- | Allows grantee to write the ACL for the applicable bucket.
poarGrantWriteACP :: Lens' PutObjectAcl (Maybe Text)
poarGrantWriteACP =
    lens _poarGrantWriteACP (\s a -> s { _poarGrantWriteACP = a })

poarKey :: Lens' PutObjectAcl Text
poarKey = lens _poarKey (\s a -> s { _poarKey = a })

instance ToPath PutObjectAcl where
    toPath PutObjectAcl{..} = mconcat
        [ "/"
        , toText _poarBucket
        , "/"
        , toText _poarKey
        ]

instance ToQuery PutObjectAcl where
    toQuery = const "acl"

instance ToHeaders PutObjectAcl where
    toHeaders PutObjectAcl{..} = mconcat
        [ "x-amz-acl"                =: _poarACL
        , "Content-MD5"              =: _poarContentMD5
        , "x-amz-grant-full-control" =: _poarGrantFullControl
        , "x-amz-grant-read"         =: _poarGrantRead
        , "x-amz-grant-read-acp"     =: _poarGrantReadACP
        , "x-amz-grant-write"        =: _poarGrantWrite
        , "x-amz-grant-write-acp"    =: _poarGrantWriteACP
        ]

instance ToBody PutObjectAcl where
    toBody = toBody . encodeXML . _poarAccessControlPolicy

data PutObjectAclResponse = PutObjectAclResponse

-- | 'PutObjectAclResponse' constructor.
putObjectAclResponse :: PutObjectAclResponse
putObjectAclResponse = PutObjectAclResponse

instance AWSRequest PutObjectAcl where
    type Sv PutObjectAcl = S3
    type Rs PutObjectAcl = PutObjectAclResponse

    request  = put'
    response = const (nullaryResponse PutObjectAclResponse)
