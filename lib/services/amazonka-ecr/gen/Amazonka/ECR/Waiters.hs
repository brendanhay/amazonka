{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECR.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.DescribeImageScanFindings
import Amazonka.ECR.GetLifecyclePolicyPreview
import Amazonka.ECR.Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.ECR.DescribeImageScanFindings' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageScanComplete :: Core.Wait DescribeImageScanFindings
newImageScanComplete =
  Core.Wait
    { Core.name = "ImageScanComplete",
      Core.attempts = 60,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETE"
            Core.AcceptSuccess
            ( describeImageScanFindingsResponse_imageScanStatus
                Prelude.. Lens._Just
                Prelude.. imageScanStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeImageScanFindingsResponse_imageScanStatus
                Prelude.. Lens._Just
                Prelude.. imageScanStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.ECR.GetLifecyclePolicyPreview' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newLifecyclePolicyPreviewComplete :: Core.Wait GetLifecyclePolicyPreview
newLifecyclePolicyPreviewComplete =
  Core.Wait
    { Core.name =
        "LifecyclePolicyPreviewComplete",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETE"
            Core.AcceptSuccess
            ( getLifecyclePolicyPreviewResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getLifecyclePolicyPreviewResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
