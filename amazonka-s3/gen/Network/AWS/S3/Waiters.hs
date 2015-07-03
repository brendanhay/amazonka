{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
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

import           Network.AWS.Prelude
import           Network.AWS.S3.HeadBucket
import           Network.AWS.S3.HeadBucket
import           Network.AWS.S3.HeadObject
import           Network.AWS.S3.HeadObject
import           Network.AWS.S3.Types
import           Network.AWS.Waiter

objectNotExists :: Wait HeadObject
objectNotExists =
    Wait
    { _waitName = "ObjectNotExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors = [matchStatus 404 AcceptSuccess]
    }

bucketExists :: Wait HeadBucket
bucketExists =
    Wait
    { _waitName = "BucketExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors = [ matchStatus 200 AcceptSuccess
                       , matchStatus 404 AcceptRetry]
    }

objectExists :: Wait HeadObject
objectExists =
    Wait
    { _waitName = "ObjectExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors = [ matchStatus 200 AcceptSuccess
                       , matchStatus 404 AcceptRetry]
    }

bucketNotExists :: Wait HeadBucket
bucketNotExists =
    Wait
    { _waitName = "BucketNotExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors = [matchStatus 404 AcceptSuccess]
    }
