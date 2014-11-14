{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
module Network.AWS.S3.PutBucketTagging
    (
    -- * Request
      PutBucketTagging
    -- ** Request constructor
    , putBucketTagging
    -- ** Request lenses
    , pbtBucket
    , pbtContentMD5
    , pbtTagging

    -- * Response
    , PutBucketTaggingResponse
    -- ** Response constructor
    , putBucketTaggingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketTagging = PutBucketTagging
    { _pbtBucket     :: Text
    , _pbtContentMD5 :: Maybe Text
    , _pbtTagging    :: Tagging
    } deriving (Eq, Show, Generic)

-- | 'PutBucketTagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbtBucket' @::@ 'Text'
--
-- * 'pbtContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbtTagging' @::@ 'Tagging'
--
putBucketTagging :: Text -- ^ 'pbtBucket'
                 -> Tagging -- ^ 'pbtTagging'
                 -> PutBucketTagging
putBucketTagging p1 p2 = PutBucketTagging
    { _pbtBucket     = p1
    , _pbtTagging    = p2
    , _pbtContentMD5 = Nothing
    }

pbtBucket :: Lens' PutBucketTagging Text
pbtBucket = lens _pbtBucket (\s a -> s { _pbtBucket = a })

pbtContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtContentMD5 = lens _pbtContentMD5 (\s a -> s { _pbtContentMD5 = a })

pbtTagging :: Lens' PutBucketTagging Tagging
pbtTagging = lens _pbtTagging (\s a -> s { _pbtTagging = a })

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = mconcat
        [ "/"
        , toText _pbtBucket
        ]

instance ToQuery PutBucketTagging where
    toQuery = const "tagging"

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} = mconcat
        [ "Content-MD5" =: _pbtContentMD5
        ]

instance ToBody PutBucketTagging where
    toBody = toBody . encodeXML . _pbtTagging

data PutBucketTaggingResponse = PutBucketTaggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketTaggingResponse' constructor.
putBucketTaggingResponse :: PutBucketTaggingResponse
putBucketTaggingResponse = PutBucketTaggingResponse

instance AWSRequest PutBucketTagging where
    type Sv PutBucketTagging = S3
    type Rs PutBucketTagging = PutBucketTaggingResponse

    request  = put
    response = nullaryResponse PutBucketTaggingResponse
