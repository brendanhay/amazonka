{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Waiters where

import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.ECR.GetLifecyclePolicyPreview' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
lifecyclePolicyPreviewComplete :: Wait GetLifecyclePolicyPreview
lifecyclePolicyPreviewComplete =
  Wait
    { _waitName = "LifecyclePolicyPreviewComplete",
      _waitAttempts = 20,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "COMPLETE" AcceptSuccess (glpprsStatus . to toTextCI),
          matchAll "FAILED" AcceptFailure (glpprsStatus . to toTextCI)
        ]
    }

-- | Polls 'Network.AWS.ECR.DescribeImageScanFindings' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
imageScanComplete :: Wait DescribeImageScanFindings
imageScanComplete =
  Wait
    { _waitName = "ImageScanComplete",
      _waitAttempts = 60,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll
            "COMPLETE"
            AcceptSuccess
            (disfrsImageScanStatus . _Just . issStatus . _Just . to toTextCI),
          matchAll
            "FAILED"
            AcceptFailure
            (disfrsImageScanStatus . _Just . issStatus . _Just . to toTextCI)
        ]
    }
