{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.HeadBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation is useful to determine if a bucket exists and you have
-- permission to access it.
module Network.AWS.S3.HeadBucket
    (
    -- * Request
      HeadBucket
    -- ** Request constructor
    , headBucket
    -- ** Request lenses
    , hbrBucket

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype HeadBucket = HeadBucket
    { _hbrBucket :: BucketName
    } deriving (Eq, Show, Generic)

-- | 'HeadBucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hbrBucket' @::@ 'BucketName'
--
headBucket :: BucketName -- ^ 'hbrBucket'
           -> HeadBucket
headBucket p1 = HeadBucket
    { _hbrBucket = p1
    }

hbrBucket :: Lens' HeadBucket BucketName
hbrBucket = lens _hbrBucket (\s a -> s { _hbrBucket = a })

instance ToPath HeadBucket where
    toPath HeadBucket{..} = mconcat
        [ "/"
        , toText _hbrBucket
        ]

instance ToQuery HeadBucket

instance ToHeaders HeadBucket


instance AWSRequest HeadBucket where
    type Sv HeadBucket = S3
    type Rs HeadBucket = Empty

    request  = head'
    response = const (nullaryResponse Empty)
