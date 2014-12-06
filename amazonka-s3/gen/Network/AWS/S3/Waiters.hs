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
import Network.AWS.Waiter

bucketExists :: Wait HeadBucket
bucketExists = Wait
    { _waitName      = "BucketExists"
    , _waitAttempts  = 20
    , _waitDelay     = 5
    , _waitAcceptors =
        [ status 200 Success
        , status 404 Retry
        ]
    }

bucketNotExists :: Wait HeadBucket
bucketNotExists = Wait
    { _waitName      = "BucketNotExists"
    , _waitAttempts  = 20
    , _waitDelay     = 5
    , _waitAcceptors =
        [ status 404 Success
        ]
    }

objectExists :: Wait HeadObject
objectExists = Wait
    { _waitName      = "ObjectExists"
    , _waitAttempts  = 20
    , _waitDelay     = 5
    , _waitAcceptors =
        [ status 200 Success
        , status 404 Retry
        ]
    }

objectNotExists :: Wait HeadObject
objectNotExists = Wait
    { _waitName      = "ObjectNotExists"
    , _waitAttempts  = 20
    , _waitDelay     = 5
    , _waitAcceptors =
        [ status 404 Success
        ]
    }
