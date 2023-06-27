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
-- Module      : Amazonka.GuardDuty.Types.UsageFeatureResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageFeatureResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.Total
import Amazonka.GuardDuty.Types.UsageFeature
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the result of the total usage based on the
-- feature.
--
-- /See:/ 'newUsageFeatureResult' smart constructor.
data UsageFeatureResult = UsageFeatureResult'
  { -- | The feature that generated the usage cost.
    feature :: Prelude.Maybe UsageFeature,
    total :: Prelude.Maybe Total
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageFeatureResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feature', 'usageFeatureResult_feature' - The feature that generated the usage cost.
--
-- 'total', 'usageFeatureResult_total' - Undocumented member.
newUsageFeatureResult ::
  UsageFeatureResult
newUsageFeatureResult =
  UsageFeatureResult'
    { feature = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The feature that generated the usage cost.
usageFeatureResult_feature :: Lens.Lens' UsageFeatureResult (Prelude.Maybe UsageFeature)
usageFeatureResult_feature = Lens.lens (\UsageFeatureResult' {feature} -> feature) (\s@UsageFeatureResult' {} a -> s {feature = a} :: UsageFeatureResult)

-- | Undocumented member.
usageFeatureResult_total :: Lens.Lens' UsageFeatureResult (Prelude.Maybe Total)
usageFeatureResult_total = Lens.lens (\UsageFeatureResult' {total} -> total) (\s@UsageFeatureResult' {} a -> s {total = a} :: UsageFeatureResult)

instance Data.FromJSON UsageFeatureResult where
  parseJSON =
    Data.withObject
      "UsageFeatureResult"
      ( \x ->
          UsageFeatureResult'
            Prelude.<$> (x Data..:? "feature")
            Prelude.<*> (x Data..:? "total")
      )

instance Prelude.Hashable UsageFeatureResult where
  hashWithSalt _salt UsageFeatureResult' {..} =
    _salt
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` total

instance Prelude.NFData UsageFeatureResult where
  rnf UsageFeatureResult' {..} =
    Prelude.rnf feature `Prelude.seq` Prelude.rnf total
