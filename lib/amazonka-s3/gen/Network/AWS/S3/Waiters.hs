{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.S3.HeadObject
import Network.AWS.S3.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
objectNotExists :: Wait HeadObject
objectNotExists =
  Wait
    { _waitName = "ObjectNotExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors = [matchStatus 404 AcceptSuccess]
    }


-- | Polls 'Network.AWS.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
bucketExists :: Wait HeadBucket
bucketExists =
  Wait
    { _waitName = "BucketExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchStatus 301 AcceptSuccess
        , matchStatus 403 AcceptSuccess
        , matchStatus 404 AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
objectExists :: Wait HeadObject
objectExists =
  Wait
    { _waitName = "ObjectExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors =
        [matchStatus 200 AcceptSuccess, matchStatus 404 AcceptRetry]
    }


-- | Polls 'Network.AWS.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
bucketNotExists :: Wait HeadBucket
bucketNotExists =
  Wait
    { _waitName = "BucketNotExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors = [matchStatus 404 AcceptSuccess]
    }

