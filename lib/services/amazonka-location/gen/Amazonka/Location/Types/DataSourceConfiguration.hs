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
-- Module      : Amazonka.Location.Types.DataSourceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.DataSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types.IntendedUse
import qualified Amazonka.Prelude as Prelude

-- | Specifies the data storage option chosen for requesting Places.
--
-- When using Amazon Location Places:
--
-- -   If using HERE Technologies as a data provider, you can\'t store
--     results for locations in Japan by setting @IntendedUse@ to
--     @Storage@. parameter.
--
-- -   Under the @MobileAssetTracking@ or @MobilAssetManagement@ pricing
--     plan, you can\'t store results from your place index resources by
--     setting @IntendedUse@ to @Storage@. This returns a validation
--     exception error.
--
-- For more information, see the
-- <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
-- Location Service.
--
-- /See:/ 'newDataSourceConfiguration' smart constructor.
data DataSourceConfiguration = DataSourceConfiguration'
  { -- | Specifies how the results of an operation will be stored by the caller.
    --
    -- Valid values include:
    --
    -- -   @SingleUse@ specifies that the results won\'t be stored.
    --
    -- -   @Storage@ specifies that the result can be cached or stored in a
    --     database.
    --
    -- Default value: @SingleUse@
    intendedUse :: Prelude.Maybe IntendedUse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intendedUse', 'dataSourceConfiguration_intendedUse' - Specifies how the results of an operation will be stored by the caller.
--
-- Valid values include:
--
-- -   @SingleUse@ specifies that the results won\'t be stored.
--
-- -   @Storage@ specifies that the result can be cached or stored in a
--     database.
--
-- Default value: @SingleUse@
newDataSourceConfiguration ::
  DataSourceConfiguration
newDataSourceConfiguration =
  DataSourceConfiguration'
    { intendedUse =
        Prelude.Nothing
    }

-- | Specifies how the results of an operation will be stored by the caller.
--
-- Valid values include:
--
-- -   @SingleUse@ specifies that the results won\'t be stored.
--
-- -   @Storage@ specifies that the result can be cached or stored in a
--     database.
--
-- Default value: @SingleUse@
dataSourceConfiguration_intendedUse :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe IntendedUse)
dataSourceConfiguration_intendedUse = Lens.lens (\DataSourceConfiguration' {intendedUse} -> intendedUse) (\s@DataSourceConfiguration' {} a -> s {intendedUse = a} :: DataSourceConfiguration)

instance Core.FromJSON DataSourceConfiguration where
  parseJSON =
    Core.withObject
      "DataSourceConfiguration"
      ( \x ->
          DataSourceConfiguration'
            Prelude.<$> (x Core..:? "IntendedUse")
      )

instance Prelude.Hashable DataSourceConfiguration where
  hashWithSalt _salt DataSourceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` intendedUse

instance Prelude.NFData DataSourceConfiguration where
  rnf DataSourceConfiguration' {..} =
    Prelude.rnf intendedUse

instance Core.ToJSON DataSourceConfiguration where
  toJSON DataSourceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("IntendedUse" Core..=) Prelude.<$> intendedUse]
      )
