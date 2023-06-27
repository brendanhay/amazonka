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
-- Module      : Amazonka.QuickSight.Types.DataSetRefreshProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetRefreshProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RefreshConfiguration

-- | The refresh properties of a dataset.
--
-- /See:/ 'newDataSetRefreshProperties' smart constructor.
data DataSetRefreshProperties = DataSetRefreshProperties'
  { -- | The refresh configuration for a dataset.
    refreshConfiguration :: RefreshConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetRefreshProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshConfiguration', 'dataSetRefreshProperties_refreshConfiguration' - The refresh configuration for a dataset.
newDataSetRefreshProperties ::
  -- | 'refreshConfiguration'
  RefreshConfiguration ->
  DataSetRefreshProperties
newDataSetRefreshProperties pRefreshConfiguration_ =
  DataSetRefreshProperties'
    { refreshConfiguration =
        pRefreshConfiguration_
    }

-- | The refresh configuration for a dataset.
dataSetRefreshProperties_refreshConfiguration :: Lens.Lens' DataSetRefreshProperties RefreshConfiguration
dataSetRefreshProperties_refreshConfiguration = Lens.lens (\DataSetRefreshProperties' {refreshConfiguration} -> refreshConfiguration) (\s@DataSetRefreshProperties' {} a -> s {refreshConfiguration = a} :: DataSetRefreshProperties)

instance Data.FromJSON DataSetRefreshProperties where
  parseJSON =
    Data.withObject
      "DataSetRefreshProperties"
      ( \x ->
          DataSetRefreshProperties'
            Prelude.<$> (x Data..: "RefreshConfiguration")
      )

instance Prelude.Hashable DataSetRefreshProperties where
  hashWithSalt _salt DataSetRefreshProperties' {..} =
    _salt `Prelude.hashWithSalt` refreshConfiguration

instance Prelude.NFData DataSetRefreshProperties where
  rnf DataSetRefreshProperties' {..} =
    Prelude.rnf refreshConfiguration

instance Data.ToJSON DataSetRefreshProperties where
  toJSON DataSetRefreshProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RefreshConfiguration"
                  Data..= refreshConfiguration
              )
          ]
      )
