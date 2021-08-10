{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of the lifecycle policy preview request.
--
-- /See:/ 'newLifecyclePolicyPreviewSummary' smart constructor.
data LifecyclePolicyPreviewSummary = LifecyclePolicyPreviewSummary'
  { -- | The number of expiring images.
    expiringImageTotalCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecyclePolicyPreviewSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiringImageTotalCount', 'lifecyclePolicyPreviewSummary_expiringImageTotalCount' - The number of expiring images.
newLifecyclePolicyPreviewSummary ::
  LifecyclePolicyPreviewSummary
newLifecyclePolicyPreviewSummary =
  LifecyclePolicyPreviewSummary'
    { expiringImageTotalCount =
        Prelude.Nothing
    }

-- | The number of expiring images.
lifecyclePolicyPreviewSummary_expiringImageTotalCount :: Lens.Lens' LifecyclePolicyPreviewSummary (Prelude.Maybe Prelude.Natural)
lifecyclePolicyPreviewSummary_expiringImageTotalCount = Lens.lens (\LifecyclePolicyPreviewSummary' {expiringImageTotalCount} -> expiringImageTotalCount) (\s@LifecyclePolicyPreviewSummary' {} a -> s {expiringImageTotalCount = a} :: LifecyclePolicyPreviewSummary)

instance Core.FromJSON LifecyclePolicyPreviewSummary where
  parseJSON =
    Core.withObject
      "LifecyclePolicyPreviewSummary"
      ( \x ->
          LifecyclePolicyPreviewSummary'
            Prelude.<$> (x Core..:? "expiringImageTotalCount")
      )

instance
  Prelude.Hashable
    LifecyclePolicyPreviewSummary

instance Prelude.NFData LifecyclePolicyPreviewSummary
