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

import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.Lens
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ECR.DescribeImageScanFindings' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageScanComplete :: Waiter.Wait DescribeImageScanFindings
newImageScanComplete =
  Waiter.Wait
    { Waiter._waitName = "ImageScanComplete",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETE"
            Waiter.AcceptSuccess
            ( describeImageScanFindingsResponse_imageScanStatus
                Prelude.. Lens._Just
                Prelude.. imageScanStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeImageScanFindingsResponse_imageScanStatus
                Prelude.. Lens._Just
                Prelude.. imageScanStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ECR.GetLifecyclePolicyPreview' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newLifecyclePolicyPreviewComplete :: Waiter.Wait GetLifecyclePolicyPreview
newLifecyclePolicyPreviewComplete =
  Waiter.Wait
    { Waiter._waitName =
        "LifecyclePolicyPreviewComplete",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETE"
            Waiter.AcceptSuccess
            ( getLifecyclePolicyPreviewResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( getLifecyclePolicyPreviewResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
