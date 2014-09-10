{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the tags for a bucket.
module Network.AWS.S3
    (
    -- * Request
      PutBucketTagging
    -- ** Request constructor
    , mkPutBucketTagging
    -- ** Request lenses
    , pbtBucket
    , pbtContentMD5
    , pbtTagging

    -- * Response
    , PutBucketTaggingResponse
    -- ** Response constructor
    , mkPutBucketTaggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketTagging = PutBucketTagging
    { _pbtBucket :: !BucketName
    , _pbtContentMD5 :: !(Maybe Text)
    , _pbtTagging :: Tagging
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketTagging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @ContentMD5 ::@ @Maybe Text@
--
-- * @Tagging ::@ @Tagging@
--
mkPutBucketTagging :: BucketName -- ^ 'pbtBucket'
                   -> Tagging -- ^ 'pbtTagging'
                   -> PutBucketTagging
mkPutBucketTagging p1 p3 = PutBucketTagging
    { _pbtBucket = p1
    , _pbtContentMD5 = Nothing
    , _pbtTagging = p3
    }

pbtBucket :: Lens' PutBucketTagging BucketName
pbtBucket = lens _pbtBucket (\s a -> s { _pbtBucket = a })

pbtContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtContentMD5 = lens _pbtContentMD5 (\s a -> s { _pbtContentMD5 = a })

pbtTagging :: Lens' PutBucketTagging Tagging
pbtTagging = lens _pbtTagging (\s a -> s { _pbtTagging = a })

instance ToPath PutBucketTagging

instance ToQuery PutBucketTagging

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} = concat
        [ "Content-MD5" =: _pbtContentMD5
        ]

instance ToBody PutBucketTagging where
    toBody = toBody . encodeXML . _pbtTagging

data PutBucketTaggingResponse = PutBucketTaggingResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketTaggingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutBucketTaggingResponse :: PutBucketTaggingResponse
mkPutBucketTaggingResponse = PutBucketTaggingResponse

instance AWSRequest PutBucketTagging where
    type Sv PutBucketTagging = S3
    type Rs PutBucketTagging = PutBucketTaggingResponse

    request = get
    response _ = nullaryResponse PutBucketTaggingResponse
