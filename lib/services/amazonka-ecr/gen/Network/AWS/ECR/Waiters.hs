{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.Lens
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.ECR.GetLifecyclePolicyPreview' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newLifecyclePolicyPreviewComplete :: Core.Wait GetLifecyclePolicyPreview
newLifecyclePolicyPreviewComplete =
  Core.Wait
    { Core._waitName =
        "LifecyclePolicyPreviewComplete",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETE"
            Core.AcceptSuccess
            ( getLifecyclePolicyPreviewResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getLifecyclePolicyPreviewResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ECR.DescribeImageScanFindings' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageScanComplete :: Core.Wait DescribeImageScanFindings
newImageScanComplete =
  Core.Wait
    { Core._waitName = "ImageScanComplete",
      Core._waitAttempts = 60,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETE"
            Core.AcceptSuccess
            ( describeImageScanFindingsResponse_imageScanStatus
                Prelude.. Lens._Just
                Prelude.. imageScanStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeImageScanFindingsResponse_imageScanStatus
                Prelude.. Lens._Just
                Prelude.. imageScanStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
