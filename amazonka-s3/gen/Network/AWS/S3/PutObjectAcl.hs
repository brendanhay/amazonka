{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutObjectAcl.html>
module Network.AWS.S3.PutObjectAcl
    (
    -- * Request
      PutObjectAcl
    -- ** Request constructor
    , putObjectAcl
    -- ** Request lenses
    , poaACL
    , poaAccessControlPolicy
    , poaBucket
    , poaContentMD5
    , poaGrantFullControl
    , poaGrantRead
    , poaGrantReadACP
    , poaGrantWrite
    , poaGrantWriteACP
    , poaKey

    -- * Response
    , PutObjectAclResponse
    -- ** Response constructor
    , putObjectAclResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutObjectAcl = PutObjectAcl
    { _poaACL                 :: Maybe Text
    , _poaAccessControlPolicy :: Maybe AccessControlPolicy
    , _poaBucket              :: Text
    , _poaContentMD5          :: Maybe Text
    , _poaGrantFullControl    :: Maybe Text
    , _poaGrantRead           :: Maybe Text
    , _poaGrantReadACP        :: Maybe Text
    , _poaGrantWrite          :: Maybe Text
    , _poaGrantWriteACP       :: Maybe Text
    , _poaKey                 :: Text
    } deriving (Eq, Show)

-- | 'PutObjectAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poaACL' @::@ 'Maybe' 'Text'
--
-- * 'poaAccessControlPolicy' @::@ 'Maybe' 'AccessControlPolicy'
--
-- * 'poaBucket' @::@ 'Text'
--
-- * 'poaContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'poaGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'poaGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'poaGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'poaGrantWrite' @::@ 'Maybe' 'Text'
--
-- * 'poaGrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'poaKey' @::@ 'Text'
--
putObjectAcl :: Text -- ^ 'poaBucket'
             -> Text -- ^ 'poaKey'
             -> PutObjectAcl
putObjectAcl p1 p2 = PutObjectAcl
    { _poaBucket              = p1
    , _poaKey                 = p2
    , _poaACL                 = Nothing
    , _poaAccessControlPolicy = Nothing
    , _poaContentMD5          = Nothing
    , _poaGrantFullControl    = Nothing
    , _poaGrantRead           = Nothing
    , _poaGrantReadACP        = Nothing
    , _poaGrantWrite          = Nothing
    , _poaGrantWriteACP       = Nothing
    }

-- | The canned ACL to apply to the object.
poaACL :: Lens' PutObjectAcl (Maybe Text)
poaACL = lens _poaACL (\s a -> s { _poaACL = a })

poaAccessControlPolicy :: Lens' PutObjectAcl (Maybe AccessControlPolicy)
poaAccessControlPolicy =
    lens _poaAccessControlPolicy (\s a -> s { _poaAccessControlPolicy = a })

poaBucket :: Lens' PutObjectAcl Text
poaBucket = lens _poaBucket (\s a -> s { _poaBucket = a })

poaContentMD5 :: Lens' PutObjectAcl (Maybe Text)
poaContentMD5 = lens _poaContentMD5 (\s a -> s { _poaContentMD5 = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
poaGrantFullControl :: Lens' PutObjectAcl (Maybe Text)
poaGrantFullControl =
    lens _poaGrantFullControl (\s a -> s { _poaGrantFullControl = a })

-- | Allows grantee to list the objects in the bucket.
poaGrantRead :: Lens' PutObjectAcl (Maybe Text)
poaGrantRead = lens _poaGrantRead (\s a -> s { _poaGrantRead = a })

-- | Allows grantee to read the bucket ACL.
poaGrantReadACP :: Lens' PutObjectAcl (Maybe Text)
poaGrantReadACP = lens _poaGrantReadACP (\s a -> s { _poaGrantReadACP = a })

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
poaGrantWrite :: Lens' PutObjectAcl (Maybe Text)
poaGrantWrite = lens _poaGrantWrite (\s a -> s { _poaGrantWrite = a })

-- | Allows grantee to write the ACL for the applicable bucket.
poaGrantWriteACP :: Lens' PutObjectAcl (Maybe Text)
poaGrantWriteACP = lens _poaGrantWriteACP (\s a -> s { _poaGrantWriteACP = a })

poaKey :: Lens' PutObjectAcl Text
poaKey = lens _poaKey (\s a -> s { _poaKey = a })

data PutObjectAclResponse = PutObjectAclResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutObjectAclResponse' constructor.
putObjectAclResponse :: PutObjectAclResponse
putObjectAclResponse = PutObjectAclResponse

instance ToPath PutObjectAcl where
    toPath PutObjectAcl{..} = mconcat
        [ "/"
        , toText _poaBucket
        , "/"
        , toText _poaKey
        ]

instance ToQuery PutObjectAcl where
    toQuery = const "acl"

instance ToHeaders PutObjectAcl where
    toHeaders PutObjectAcl{..} = mconcat
        [ "x-amz-acl"                =: _poaACL
        , "Content-MD5"              =: _poaContentMD5
        , "x-amz-grant-full-control" =: _poaGrantFullControl
        , "x-amz-grant-read"         =: _poaGrantRead
        , "x-amz-grant-read-acp"     =: _poaGrantReadACP
        , "x-amz-grant-write"        =: _poaGrantWrite
        , "x-amz-grant-write-acp"    =: _poaGrantWriteACP
        ]

instance ToXMLRoot PutObjectAcl where
    toXMLRoot PutObjectAcl{..} = element "PutObjectAcl"
        [ "AccessControlPolicy" =@ _poaAccessControlPolicy
        ]

instance ToXML PutObjectAcl

instance AWSRequest PutObjectAcl where
    type Sv PutObjectAcl = S3
    type Rs PutObjectAcl = PutObjectAclResponse

    request  = put
    response = nullResponse PutObjectAclResponse


Some kind of operator / class to check the types whether to continue?
