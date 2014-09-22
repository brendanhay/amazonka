{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutObjectAcl = PutObjectAcl
    { _poaACL :: Maybe ObjectCannedACL
    , _poaAccessControlPolicy :: Maybe AccessControlPolicy
    , _poaBucket :: Text
    , _poaContentMD5 :: Maybe Text
    , _poaGrantFullControl :: Maybe Text
    , _poaGrantRead :: Maybe Text
    , _poaGrantReadACP :: Maybe Text
    , _poaGrantWrite :: Maybe Text
    , _poaGrantWriteACP :: Maybe Text
    , _poaKey :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutObjectAcl' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ACL ::@ @Maybe ObjectCannedACL@
--
-- * @AccessControlPolicy ::@ @Maybe AccessControlPolicy@
--
-- * @Bucket ::@ @Text@
--
-- * @ContentMD5 ::@ @Maybe Text@
--
-- * @GrantFullControl ::@ @Maybe Text@
--
-- * @GrantRead ::@ @Maybe Text@
--
-- * @GrantReadACP ::@ @Maybe Text@
--
-- * @GrantWrite ::@ @Maybe Text@
--
-- * @GrantWriteACP ::@ @Maybe Text@
--
-- * @Key ::@ @Text@
--
putObjectAcl :: Text -- ^ 'poaKey'
             -> Text -- ^ 'poaBucket'
             -> PutObjectAcl
putObjectAcl p10 p3 = PutObjectAcl
    { _poaACL = Nothing
    , _poaAccessControlPolicy = Nothing
    , _poaBucket = p3
    , _poaContentMD5 = Nothing
    , _poaGrantFullControl = Nothing
    , _poaGrantRead = Nothing
    , _poaGrantReadACP = Nothing
    , _poaGrantWrite = Nothing
    , _poaGrantWriteACP = Nothing
    , _poaKey = p10
    }

-- | The canned ACL to apply to the object.
poaACL :: Lens' PutObjectAcl (Maybe ObjectCannedACL)
poaACL = lens _poaACL (\s a -> s { _poaACL = a })

poaAccessControlPolicy :: Lens' PutObjectAcl (Maybe AccessControlPolicy)
poaAccessControlPolicy =
    lens _poaAccessControlPolicy (\s a -> s { _poaAccessControlPolicy = a })

poaBucket :: Lens' PutObjectAcl Text
poaBucket = lens _poaBucket (\s a -> s { _poaBucket = a })

poaContentMD5 :: Lens' PutObjectAcl (Maybe Text)
poaContentMD5 = lens _poaContentMD5 (\s a -> s { _poaContentMD5 = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the
-- bucket.
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
poaGrantWriteACP =
    lens _poaGrantWriteACP (\s a -> s { _poaGrantWriteACP = a })

poaKey :: Lens' PutObjectAcl Text
poaKey = lens _poaKey (\s a -> s { _poaKey = a })

instance ToPath PutObjectAcl

instance ToQuery PutObjectAcl

instance ToHeaders PutObjectAcl where
    toHeaders PutObjectAcl{..} = concat
        [ "x-amz-acl" =: _poaACL
        , "Content-MD5" =: _poaContentMD5
        , "x-amz-grant-full-control" =: _poaGrantFullControl
        , "x-amz-grant-read" =: _poaGrantRead
        , "x-amz-grant-read-acp" =: _poaGrantReadACP
        , "x-amz-grant-write" =: _poaGrantWrite
        , "x-amz-grant-write-acp" =: _poaGrantWriteACP
        ]

instance ToBody PutObjectAcl where
    toBody = toBody . encodeXML . _poaAccessControlPolicy

data PutObjectAclResponse = PutObjectAclResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutObjectAclResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putObjectAclResponse :: PutObjectAclResponse
putObjectAclResponse = PutObjectAclResponse

instance AWSRequest PutObjectAcl where
    type Sv PutObjectAcl = S3
    type Rs PutObjectAcl = PutObjectAclResponse

    request = get
    response _ = nullaryResponse PutObjectAclResponse
