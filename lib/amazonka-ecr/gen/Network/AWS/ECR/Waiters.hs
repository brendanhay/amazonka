{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Waiters
  ( -- * LifecyclePolicyPreviewComplete
    mkLifecyclePolicyPreviewComplete,

    -- * ImageScanComplete
    mkImageScanComplete,
  )
where

import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ECR.GetLifecyclePolicyPreview' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkLifecyclePolicyPreviewComplete :: Wait.Wait GetLifecyclePolicyPreview
mkLifecyclePolicyPreviewComplete =
  Wait.Wait
    { Wait._waitName = "LifecyclePolicyPreviewComplete",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETE"
            Wait.AcceptSuccess
            (glpprsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            (glpprsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.ECR.DescribeImageScanFindings' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkImageScanComplete :: Wait.Wait DescribeImageScanFindings
mkImageScanComplete =
  Wait.Wait
    { Wait._waitName = "ImageScanComplete",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETE"
            Wait.AcceptSuccess
            ( disfrsImageScanStatus Lude.. Lens._Just
                Lude.. issStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            ( disfrsImageScanStatus Lude.. Lens._Just
                Lude.. issStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
