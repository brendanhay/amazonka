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
-- Module      : Amazonka.ECR.Types.LifecyclePolicyPreviewSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.LifecyclePolicyPreviewSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON LifecyclePolicyPreviewSummary where
  parseJSON =
    Data.withObject
      "LifecyclePolicyPreviewSummary"
      ( \x ->
          LifecyclePolicyPreviewSummary'
            Prelude.<$> (x Data..:? "expiringImageTotalCount")
      )

instance
  Prelude.Hashable
    LifecyclePolicyPreviewSummary
  where
  hashWithSalt _salt LifecyclePolicyPreviewSummary' {..} =
    _salt
      `Prelude.hashWithSalt` expiringImageTotalCount

instance Prelude.NFData LifecyclePolicyPreviewSummary where
  rnf LifecyclePolicyPreviewSummary' {..} =
    Prelude.rnf expiringImageTotalCount
