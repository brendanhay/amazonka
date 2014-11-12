{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , pbaACL
    , pbaAccessControlPolicy
    , pbaBucket
    , pbaContentMD5
    , pbaGrantFullControl
    , pbaGrantRead
    , pbaGrantReadACP
    , pbaGrantWrite
    , pbaGrantWriteACP

    -- * Response
    , PutBucketAclResponse
    -- ** Response constructor
    , putBucketAclResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutBucketAcl = PutBucketAcl
    { _pbaACL                 :: Maybe Text
    , _pbaAccessControlPolicy :: Maybe AccessControlPolicy
    , _pbaBucket              :: Text
    , _pbaContentMD5          :: Maybe Text
    , _pbaGrantFullControl    :: Maybe Text
    , _pbaGrantRead           :: Maybe Text
    , _pbaGrantReadACP        :: Maybe Text
    , _pbaGrantWrite          :: Maybe Text
    , _pbaGrantWriteACP       :: Maybe Text
    } (Eq, Show, Generic)

-- | 'PutBucketAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbaACL' @::@ 'Maybe' 'Text'
--
-- * 'pbaAccessControlPolicy' @::@ 'Maybe' 'AccessControlPolicy'
--
-- * 'pbaBucket' @::@ 'Text'
--
-- * 'pbaContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbaGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'pbaGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'pbaGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'pbaGrantWrite' @::@ 'Maybe' 'Text'
--
-- * 'pbaGrantWriteACP' @::@ 'Maybe' 'Text'
--
putBucketAcl :: Text -- ^ 'pbaBucket'
             -> PutBucketAcl
putBucketAcl p1 = PutBucketAcl
    { _pbaBucket              = p1
    , _pbaACL                 = Nothing
    , _pbaAccessControlPolicy = Nothing
    , _pbaContentMD5          = Nothing
    , _pbaGrantFullControl    = Nothing
    , _pbaGrantRead           = Nothing
    , _pbaGrantReadACP        = Nothing
    , _pbaGrantWrite          = Nothing
    , _pbaGrantWriteACP       = Nothing
    }

-- | The canned ACL to apply to the bucket.
pbaACL :: Lens' PutBucketAcl (Maybe Text)
pbaACL = lens _pbaACL (\s a -> s { _pbaACL = a })

pbaAccessControlPolicy :: Lens' PutBucketAcl (Maybe AccessControlPolicy)
pbaAccessControlPolicy =
    lens _pbaAccessControlPolicy (\s a -> s { _pbaAccessControlPolicy = a })

pbaBucket :: Lens' PutBucketAcl Text
pbaBucket = lens _pbaBucket (\s a -> s { _pbaBucket = a })

pbaContentMD5 :: Lens' PutBucketAcl (Maybe Text)
pbaContentMD5 = lens _pbaContentMD5 (\s a -> s { _pbaContentMD5 = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
pbaGrantFullControl :: Lens' PutBucketAcl (Maybe Text)
pbaGrantFullControl =
    lens _pbaGrantFullControl (\s a -> s { _pbaGrantFullControl = a })

-- | Allows grantee to list the objects in the bucket.
pbaGrantRead :: Lens' PutBucketAcl (Maybe Text)
pbaGrantRead = lens _pbaGrantRead (\s a -> s { _pbaGrantRead = a })

-- | Allows grantee to read the bucket ACL.
pbaGrantReadACP :: Lens' PutBucketAcl (Maybe Text)
pbaGrantReadACP = lens _pbaGrantReadACP (\s a -> s { _pbaGrantReadACP = a })

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
pbaGrantWrite :: Lens' PutBucketAcl (Maybe Text)
pbaGrantWrite = lens _pbaGrantWrite (\s a -> s { _pbaGrantWrite = a })

-- | Allows grantee to write the ACL for the applicable bucket.
pbaGrantWriteACP :: Lens' PutBucketAcl (Maybe Text)
pbaGrantWriteACP = lens _pbaGrantWriteACP (\s a -> s { _pbaGrantWriteACP = a })

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = mconcat
        [ "/"
        , toText _pbaBucket
        ]

instance ToQuery PutBucketAcl where
    toQuery = const "acl"

instance ToHeaders PutBucketAcl where
    toHeaders PutBucketAcl{..} = mconcat
        [ "x-amz-acl"                =: _pbaACL
        , "Content-MD5"              =: _pbaContentMD5
        , "x-amz-grant-full-control" =: _pbaGrantFullControl
        , "x-amz-grant-read"         =: _pbaGrantRead
        , "x-amz-grant-read-acp"     =: _pbaGrantReadACP
        , "x-amz-grant-write"        =: _pbaGrantWrite
        , "x-amz-grant-write-acp"    =: _pbaGrantWriteACP
        ]

instance ToBody PutBucketAcl where
    toBody = toBody . encodeXML . _pbaAccessControlPolicy

data PutBucketAclResponse = PutBucketAclResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketAclResponse' constructor.
putBucketAclResponse :: PutBucketAclResponse
putBucketAclResponse = PutBucketAclResponse

instance FromXML PutBucketAclResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutBucketAclResponse"
instance AWSRequest PutBucketAcl where
    type Sv PutBucketAcl = S3
    type Rs PutBucketAcl = PutBucketAclResponse

    request  = put
    response = nullaryResponse PutBucketAclResponse
