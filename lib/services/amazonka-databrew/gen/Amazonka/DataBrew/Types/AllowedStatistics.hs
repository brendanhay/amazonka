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
-- Module      : Amazonka.DataBrew.Types.AllowedStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.AllowedStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration of statistics that are allowed to be run on columns that
-- contain detected entities. When undefined, no statistics will be
-- computed on columns that contain detected entities.
--
-- /See:/ 'newAllowedStatistics' smart constructor.
data AllowedStatistics = AllowedStatistics'
  { -- | One or more column statistics to allow for columns that contain detected
    -- entities.
    statistics :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowedStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statistics', 'allowedStatistics_statistics' - One or more column statistics to allow for columns that contain detected
-- entities.
newAllowedStatistics ::
  -- | 'statistics'
  Prelude.NonEmpty Prelude.Text ->
  AllowedStatistics
newAllowedStatistics pStatistics_ =
  AllowedStatistics'
    { statistics =
        Lens.coerced Lens.# pStatistics_
    }

-- | One or more column statistics to allow for columns that contain detected
-- entities.
allowedStatistics_statistics :: Lens.Lens' AllowedStatistics (Prelude.NonEmpty Prelude.Text)
allowedStatistics_statistics = Lens.lens (\AllowedStatistics' {statistics} -> statistics) (\s@AllowedStatistics' {} a -> s {statistics = a} :: AllowedStatistics) Prelude.. Lens.coerced

instance Core.FromJSON AllowedStatistics where
  parseJSON =
    Core.withObject
      "AllowedStatistics"
      ( \x ->
          AllowedStatistics'
            Prelude.<$> (x Core..: "Statistics")
      )

instance Prelude.Hashable AllowedStatistics where
  hashWithSalt _salt AllowedStatistics' {..} =
    _salt `Prelude.hashWithSalt` statistics

instance Prelude.NFData AllowedStatistics where
  rnf AllowedStatistics' {..} = Prelude.rnf statistics

instance Core.ToJSON AllowedStatistics where
  toJSON AllowedStatistics' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Statistics" Core..= statistics)]
      )
