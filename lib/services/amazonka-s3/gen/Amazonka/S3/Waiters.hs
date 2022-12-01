{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.HeadBucket
import Amazonka.S3.HeadObject
import Amazonka.S3.Lens
import Amazonka.S3.Types

-- | Polls 'Amazonka.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newObjectNotExists :: Core.Wait HeadObject
newObjectNotExists =
  Core.Wait
    { Core.name = "ObjectNotExists",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [Core.matchStatus 404 Core.AcceptSuccess]
    }

-- | Polls 'Amazonka.S3.HeadObject' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newObjectExists :: Core.Wait HeadObject
newObjectExists =
  Core.Wait
    { Core.name = "ObjectExists",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newBucketNotExists :: Core.Wait HeadBucket
newBucketNotExists =
  Core.Wait
    { Core.name = "BucketNotExists",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [Core.matchStatus 404 Core.AcceptSuccess]
    }

-- | Polls 'Amazonka.S3.HeadBucket' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newBucketExists :: Core.Wait HeadBucket
newBucketExists =
  Core.Wait
    { Core.name = "BucketExists",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 301 Core.AcceptSuccess,
          Core.matchStatus 403 Core.AcceptSuccess,
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }
