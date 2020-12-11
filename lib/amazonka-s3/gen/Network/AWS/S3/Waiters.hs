{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Waiters
  ( -- * ObjectNotExists
    mkObjectNotExists,

    -- * BucketExists
    mkBucketExists,

    -- * ObjectExists
    mkObjectExists,

    -- * BucketNotExists
    mkBucketNotExists,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.S3.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkObjectNotExists :: Wait.Wait HeadObject
mkObjectNotExists =
  Wait.Wait
    { Wait._waitName = "ObjectNotExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors = [Wait.matchStatus 404 Wait.AcceptSuccess]
    }

-- | Polls 'Network.AWS.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkBucketExists :: Wait.Wait HeadBucket
mkBucketExists =
  Wait.Wait
    { Wait._waitName = "BucketExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchStatus 301 Wait.AcceptSuccess,
          Wait.matchStatus 403 Wait.AcceptSuccess,
          Wait.matchStatus 404 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkObjectExists :: Wait.Wait HeadObject
mkObjectExists =
  Wait.Wait
    { Wait._waitName = "ObjectExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchStatus 404 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkBucketNotExists :: Wait.Wait HeadBucket
mkBucketNotExists =
  Wait.Wait
    { Wait._waitName = "BucketNotExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors = [Wait.matchStatus 404 Wait.AcceptSuccess]
    }
