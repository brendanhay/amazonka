{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all buckets owned by the authenticated sender of the
-- request.
module Network.AWS.S3.ListBuckets
    (
    -- * Request
      Empty
    -- * Response
    , ListBucketsOutput
    -- ** Response constructor
    , listBucketsOutput
    -- ** Response lenses
    , lboBuckets
    , lboOwner
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types


instance ToPath Empty where
    toPath = const "/"

instance ToQuery Empty

instance ToHeaders Empty

data ListBucketsOutput = ListBucketsOutput
    { _lboBuckets :: [Bucket]
    , _lboOwner   :: Maybe Owner
    } deriving (Eq, Show, Generic)

-- | 'ListBucketsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lboBuckets' @::@ ['Bucket']
--
-- * 'lboOwner' @::@ 'Maybe' 'Owner'
--
listBucketsOutput :: ListBucketsOutput
listBucketsOutput = ListBucketsOutput
    { _lboBuckets = mempty
    , _lboOwner   = Nothing
    }

lboBuckets :: Lens' ListBucketsOutput [Bucket]
lboBuckets = lens _lboBuckets (\s a -> s { _lboBuckets = a })

lboOwner :: Lens' ListBucketsOutput (Maybe Owner)
lboOwner = lens _lboOwner (\s a -> s { _lboOwner = a })

instance AWSRequest Empty where
    type Sv Empty = S3
    type Rs Empty = ListBucketsOutput

    request  = get'
    response = const . xmlResponse $ \h x -> ListBucketsOutput
        <$> x %| "Buckets"
        <*> x %| "Owner"
