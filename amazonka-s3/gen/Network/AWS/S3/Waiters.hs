{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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

bucketExists :: Wait HeadBucket
bucketExists = Wait
    { _waitName     = "BucketExists"
    , _waitDelay    = 5
    , _waitAttempts = 20
    , _waitAccept   = const True
    }

bucketNotExists :: Wait HeadBucket
bucketNotExists = Wait
    { _waitName     = "BucketNotExists"
    , _waitDelay    = 5
    , _waitAttempts = 20
    , _waitAccept   = const True
    }

objectExists :: Wait HeadObject
objectExists = Wait
    { _waitName     = "ObjectExists"
    , _waitDelay    = 5
    , _waitAttempts = 20
    , _waitAccept   = const True
    }

objectNotExists :: Wait HeadObject
objectNotExists = Wait
    { _waitName     = "ObjectNotExists"
    , _waitDelay    = 5
    , _waitAttempts = 20
    , _waitAccept   = const True
    }
