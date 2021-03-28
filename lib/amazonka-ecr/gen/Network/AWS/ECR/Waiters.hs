{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Waiters
  (
    -- * LifecyclePolicyPreviewComplete
    mkLifecyclePolicyPreviewComplete,
    -- * ImageScanComplete
    mkImageScanComplete,
  ) where

import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.GetLifecyclePolicyPreview
import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ECR.GetLifecyclePolicyPreview' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkLifecyclePolicyPreviewComplete :: Waiter.Wait GetLifecyclePolicyPreview
mkLifecyclePolicyPreviewComplete
  = Waiter.Wait{Waiter._waitName = "LifecyclePolicyPreviewComplete",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "COMPLETE" Waiter.AcceptSuccess
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "FAILED" Waiter.AcceptFailure
                     (Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.ECR.DescribeImageScanFindings' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkImageScanComplete :: Waiter.Wait DescribeImageScanFindings
mkImageScanComplete
  = Waiter.Wait{Waiter._waitName = "ImageScanComplete",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "COMPLETE" Waiter.AcceptSuccess
                     (Lens.field @"imageScanStatus" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "FAILED" Waiter.AcceptFailure
                     (Lens.field @"imageScanStatus" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just)]}
