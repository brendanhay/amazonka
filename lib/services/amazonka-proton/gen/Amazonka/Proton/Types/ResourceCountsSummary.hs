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
-- Module      : Amazonka.Proton.Types.ResourceCountsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ResourceCountsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary counts of each Proton resource types.
--
-- /See:/ 'newResourceCountsSummary' smart constructor.
data ResourceCountsSummary = ResourceCountsSummary'
  { -- | The number of resources of this type in the Amazon Web Services account
    -- that need a major template version update.
    behindMajor :: Prelude.Maybe Prelude.Int,
    -- | The number of resources of this type in the Amazon Web Services account
    -- that need a minor template version update.
    behindMinor :: Prelude.Maybe Prelude.Int,
    -- | The number of resources of this type in the Amazon Web Services account
    -- that failed to deploy.
    failed :: Prelude.Maybe Prelude.Int,
    -- | The number of resources of this type in the Amazon Web Services account
    -- that are up-to-date with their template.
    upToDate :: Prelude.Maybe Prelude.Int,
    -- | The total number of resources of this type in the Amazon Web Services
    -- account.
    total :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCountsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behindMajor', 'resourceCountsSummary_behindMajor' - The number of resources of this type in the Amazon Web Services account
-- that need a major template version update.
--
-- 'behindMinor', 'resourceCountsSummary_behindMinor' - The number of resources of this type in the Amazon Web Services account
-- that need a minor template version update.
--
-- 'failed', 'resourceCountsSummary_failed' - The number of resources of this type in the Amazon Web Services account
-- that failed to deploy.
--
-- 'upToDate', 'resourceCountsSummary_upToDate' - The number of resources of this type in the Amazon Web Services account
-- that are up-to-date with their template.
--
-- 'total', 'resourceCountsSummary_total' - The total number of resources of this type in the Amazon Web Services
-- account.
newResourceCountsSummary ::
  -- | 'total'
  Prelude.Int ->
  ResourceCountsSummary
newResourceCountsSummary pTotal_ =
  ResourceCountsSummary'
    { behindMajor =
        Prelude.Nothing,
      behindMinor = Prelude.Nothing,
      failed = Prelude.Nothing,
      upToDate = Prelude.Nothing,
      total = pTotal_
    }

-- | The number of resources of this type in the Amazon Web Services account
-- that need a major template version update.
resourceCountsSummary_behindMajor :: Lens.Lens' ResourceCountsSummary (Prelude.Maybe Prelude.Int)
resourceCountsSummary_behindMajor = Lens.lens (\ResourceCountsSummary' {behindMajor} -> behindMajor) (\s@ResourceCountsSummary' {} a -> s {behindMajor = a} :: ResourceCountsSummary)

-- | The number of resources of this type in the Amazon Web Services account
-- that need a minor template version update.
resourceCountsSummary_behindMinor :: Lens.Lens' ResourceCountsSummary (Prelude.Maybe Prelude.Int)
resourceCountsSummary_behindMinor = Lens.lens (\ResourceCountsSummary' {behindMinor} -> behindMinor) (\s@ResourceCountsSummary' {} a -> s {behindMinor = a} :: ResourceCountsSummary)

-- | The number of resources of this type in the Amazon Web Services account
-- that failed to deploy.
resourceCountsSummary_failed :: Lens.Lens' ResourceCountsSummary (Prelude.Maybe Prelude.Int)
resourceCountsSummary_failed = Lens.lens (\ResourceCountsSummary' {failed} -> failed) (\s@ResourceCountsSummary' {} a -> s {failed = a} :: ResourceCountsSummary)

-- | The number of resources of this type in the Amazon Web Services account
-- that are up-to-date with their template.
resourceCountsSummary_upToDate :: Lens.Lens' ResourceCountsSummary (Prelude.Maybe Prelude.Int)
resourceCountsSummary_upToDate = Lens.lens (\ResourceCountsSummary' {upToDate} -> upToDate) (\s@ResourceCountsSummary' {} a -> s {upToDate = a} :: ResourceCountsSummary)

-- | The total number of resources of this type in the Amazon Web Services
-- account.
resourceCountsSummary_total :: Lens.Lens' ResourceCountsSummary Prelude.Int
resourceCountsSummary_total = Lens.lens (\ResourceCountsSummary' {total} -> total) (\s@ResourceCountsSummary' {} a -> s {total = a} :: ResourceCountsSummary)

instance Data.FromJSON ResourceCountsSummary where
  parseJSON =
    Data.withObject
      "ResourceCountsSummary"
      ( \x ->
          ResourceCountsSummary'
            Prelude.<$> (x Data..:? "behindMajor")
            Prelude.<*> (x Data..:? "behindMinor")
            Prelude.<*> (x Data..:? "failed")
            Prelude.<*> (x Data..:? "upToDate")
            Prelude.<*> (x Data..: "total")
      )

instance Prelude.Hashable ResourceCountsSummary where
  hashWithSalt _salt ResourceCountsSummary' {..} =
    _salt
      `Prelude.hashWithSalt` behindMajor
      `Prelude.hashWithSalt` behindMinor
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` upToDate
      `Prelude.hashWithSalt` total

instance Prelude.NFData ResourceCountsSummary where
  rnf ResourceCountsSummary' {..} =
    Prelude.rnf behindMajor
      `Prelude.seq` Prelude.rnf behindMinor
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf upToDate
      `Prelude.seq` Prelude.rnf total
