{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.S3.Lens
import Network.AWS.S3.Types

-- | Polls 'Network.AWS.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newBucketNotExists :: Core.Wait HeadBucket
newBucketNotExists =
  Core.Wait
    { Core._waitName = "BucketNotExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [Core.matchStatus 404 Core.AcceptSuccess]
    }

-- | Polls 'Network.AWS.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newObjectExists :: Core.Wait HeadObject
newObjectExists =
  Core.Wait
    { Core._waitName = "ObjectExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newBucketExists :: Core.Wait HeadBucket
newBucketExists =
  Core.Wait
    { Core._waitName = "BucketExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 301 Core.AcceptSuccess,
          Core.matchStatus 403 Core.AcceptSuccess,
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newObjectNotExists :: Core.Wait HeadObject
newObjectNotExists =
  Core.Wait
    { Core._waitName = "ObjectNotExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [Core.matchStatus 404 Core.AcceptSuccess]
    }
