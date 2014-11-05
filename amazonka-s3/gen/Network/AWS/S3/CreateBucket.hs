{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new bucket.
module Network.AWS.S3.CreateBucket
    (
    -- * Request
      CreateBucket
    -- ** Request constructor
    , createBucket
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
    , CreateBucketOutput
    -- ** Response constructor
    , createBucketOutput
    -- ** Response lenses
    , cboLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data CreateBucket = CreateBucket
    { _cbrACL                       :: Maybe Text
    , _cbrBucket                    :: BucketName
    , _cbrCreateBucketConfiguration :: Maybe CreateBucketConfiguration
    , _cbrGrantFullControl          :: Maybe Text
    , _cbrGrantRead                 :: Maybe Text
    , _cbrGrantReadACP              :: Maybe Text
    , _cbrGrantWrite                :: Maybe Text
    , _cbrGrantWriteACP             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateBucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbrACL' @::@ 'Maybe' 'Text'
--
-- * 'cbrBucket' @::@ 'BucketName'
--
-- * 'cbrCreateBucketConfiguration' @::@ 'Maybe' 'CreateBucketConfiguration'
--
-- * 'cbrGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'cbrGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'cbrGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'cbrGrantWrite' @::@ 'Maybe' 'Text'
--
-- * 'cbrGrantWriteACP' @::@ 'Maybe' 'Text'
--
createBucket :: BucketName -- ^ 'cbrBucket'
             -> CreateBucket
createBucket p1 = CreateBucket
    { _cbrBucket                    = p1
    , _cbrACL                       = Nothing
    , _cbrCreateBucketConfiguration = Nothing
    , _cbrGrantFullControl          = Nothing
    , _cbrGrantRead                 = Nothing
    , _cbrGrantReadACP              = Nothing
    , _cbrGrantWrite                = Nothing
    , _cbrGrantWriteACP             = Nothing
    }

-- | The canned ACL to apply to the bucket.
cbrACL :: Lens' CreateBucket (Maybe Text)
cbrACL = lens _cbrACL (\s a -> s { _cbrACL = a })

cbrBucket :: Lens' CreateBucket BucketName
cbrBucket = lens _cbrBucket (\s a -> s { _cbrBucket = a })

cbrCreateBucketConfiguration :: Lens' CreateBucket (Maybe CreateBucketConfiguration)
cbrCreateBucketConfiguration =
    lens _cbrCreateBucketConfiguration
        (\s a -> s { _cbrCreateBucketConfiguration = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
cbrGrantFullControl :: Lens' CreateBucket (Maybe Text)
cbrGrantFullControl =
    lens _cbrGrantFullControl (\s a -> s { _cbrGrantFullControl = a })

-- | Allows grantee to list the objects in the bucket.
cbrGrantRead :: Lens' CreateBucket (Maybe Text)
cbrGrantRead = lens _cbrGrantRead (\s a -> s { _cbrGrantRead = a })

-- | Allows grantee to read the bucket ACL.
cbrGrantReadACP :: Lens' CreateBucket (Maybe Text)
cbrGrantReadACP = lens _cbrGrantReadACP (\s a -> s { _cbrGrantReadACP = a })

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
cbrGrantWrite :: Lens' CreateBucket (Maybe Text)
cbrGrantWrite = lens _cbrGrantWrite (\s a -> s { _cbrGrantWrite = a })

-- | Allows grantee to write the ACL for the applicable bucket.
cbrGrantWriteACP :: Lens' CreateBucket (Maybe Text)
cbrGrantWriteACP = lens _cbrGrantWriteACP (\s a -> s { _cbrGrantWriteACP = a })

instance ToPath CreateBucket where
    toPath CreateBucket{..} = mconcat
        [ "/"
        , toText _cbrBucket
        ]

instance ToQuery CreateBucket

instance ToHeaders CreateBucket where
    toHeaders CreateBucket{..} = mconcat
        [ "x-amz-acl"                =: _cbrACL
        , "x-amz-grant-full-control" =: _cbrGrantFullControl
        , "x-amz-grant-read"         =: _cbrGrantRead
        , "x-amz-grant-read-acp"     =: _cbrGrantReadACP
        , "x-amz-grant-write"        =: _cbrGrantWrite
        , "x-amz-grant-write-acp"    =: _cbrGrantWriteACP
        ]

newtype CreateBucketOutput = CreateBucketOutput
    { _cboLocation :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

instance AWSRequest CreateBucket where
    type Sv CreateBucket = S3
    type Rs CreateBucket = CreateBucketOutput

    request  = put
    response = const . xmlResponse $ \h x ->
        <$> h ~: "Location"
