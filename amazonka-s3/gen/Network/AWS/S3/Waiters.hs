{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.S3.Waiters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.S3.Waiters where

import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.Types

data BucketExists = BucketExists
    deriving (Show)

instance AWSWaiter BucketExists where
    type Rq BucketExists = HeadBucket

    waiter BucketExists = Waiter
        { _waitDelay     = 5
        , _waitAttempts  = 20
        , _waitAccept    = const True
        }

data BucketNotExists = BucketNotExists
    deriving (Show)

instance AWSWaiter BucketNotExists where
    type Rq BucketNotExists = HeadBucket

    waiter BucketNotExists = Waiter
        { _waitDelay     = 5
        , _waitAttempts  = 20
        , _waitAccept    = const True
        }

data ObjectExists = ObjectExists
    deriving (Show)

instance AWSWaiter ObjectExists where
    type Rq ObjectExists = HeadObject

    waiter ObjectExists = Waiter
        { _waitDelay     = 5
        , _waitAttempts  = 20
        , _waitAccept    = const True
        }

data ObjectNotExists = ObjectNotExists
    deriving (Show)

instance AWSWaiter ObjectNotExists where
    type Rq ObjectNotExists = HeadObject

    waiter ObjectNotExists = Waiter
        { _waitDelay     = 5
        , _waitAttempts  = 20
        , _waitAccept    = const True
        }
