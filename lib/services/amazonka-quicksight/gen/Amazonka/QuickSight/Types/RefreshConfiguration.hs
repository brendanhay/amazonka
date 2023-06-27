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
-- Module      : Amazonka.QuickSight.Types.RefreshConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RefreshConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.IncrementalRefresh

-- | The refresh configuration of a dataset.
--
-- /See:/ 'newRefreshConfiguration' smart constructor.
data RefreshConfiguration = RefreshConfiguration'
  { -- | The incremental refresh for the dataset.
    incrementalRefresh :: IncrementalRefresh
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'incrementalRefresh', 'refreshConfiguration_incrementalRefresh' - The incremental refresh for the dataset.
newRefreshConfiguration ::
  -- | 'incrementalRefresh'
  IncrementalRefresh ->
  RefreshConfiguration
newRefreshConfiguration pIncrementalRefresh_ =
  RefreshConfiguration'
    { incrementalRefresh =
        pIncrementalRefresh_
    }

-- | The incremental refresh for the dataset.
refreshConfiguration_incrementalRefresh :: Lens.Lens' RefreshConfiguration IncrementalRefresh
refreshConfiguration_incrementalRefresh = Lens.lens (\RefreshConfiguration' {incrementalRefresh} -> incrementalRefresh) (\s@RefreshConfiguration' {} a -> s {incrementalRefresh = a} :: RefreshConfiguration)

instance Data.FromJSON RefreshConfiguration where
  parseJSON =
    Data.withObject
      "RefreshConfiguration"
      ( \x ->
          RefreshConfiguration'
            Prelude.<$> (x Data..: "IncrementalRefresh")
      )

instance Prelude.Hashable RefreshConfiguration where
  hashWithSalt _salt RefreshConfiguration' {..} =
    _salt `Prelude.hashWithSalt` incrementalRefresh

instance Prelude.NFData RefreshConfiguration where
  rnf RefreshConfiguration' {..} =
    Prelude.rnf incrementalRefresh

instance Data.ToJSON RefreshConfiguration where
  toJSON RefreshConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IncrementalRefresh" Data..= incrementalRefresh)
          ]
      )
