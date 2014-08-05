{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListBuckets
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
module Network.AWS.S3.V2006_03_01.ListBuckets where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type GetService = ListBuckets

data ListBuckets = ListBuckets
    deriving (Eq, Show, Generic)

makeLenses ''ListBuckets

instance ToPath ListBuckets where
    toPath = const "/"

instance ToQuery ListBuckets

instance ToHeaders ListBuckets

instance ToBody ListBuckets

data ListBucketsResponse = ListBucketsResponse
    { _lboBuckets :: [Bucket]
    , _lboOwner :: Maybe Owner
    } deriving (Show, Generic)

makeLenses ''ListBucketsResponse

instance AWSRequest ListBuckets where
    type Sv ListBuckets = S3
    type Rs ListBuckets = ListBucketsResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure ListBucketsResponse
            <*> xml %| "Buckets"
            <*> xml %|? "Owner"
