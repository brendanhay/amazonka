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
-- Module      : Amazonka.OpenSearch.Types.Limits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.Limits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.AdditionalLimit
import Amazonka.OpenSearch.Types.InstanceLimits
import Amazonka.OpenSearch.Types.StorageType
import qualified Amazonka.Prelude as Prelude

-- | Limits for a given instance type and for each of its roles.
--
-- /See:/ 'newLimits' smart constructor.
data Limits = Limits'
  { -- | The limits for a given instance type.
    instanceLimits :: Prelude.Maybe InstanceLimits,
    -- | Storage-related attributes that are available for a given instance type.
    storageTypes :: Prelude.Maybe [StorageType],
    -- | List of additional limits that are specific to a given instance type for
    -- each of its instance roles.
    additionalLimits :: Prelude.Maybe [AdditionalLimit]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Limits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceLimits', 'limits_instanceLimits' - The limits for a given instance type.
--
-- 'storageTypes', 'limits_storageTypes' - Storage-related attributes that are available for a given instance type.
--
-- 'additionalLimits', 'limits_additionalLimits' - List of additional limits that are specific to a given instance type for
-- each of its instance roles.
newLimits ::
  Limits
newLimits =
  Limits'
    { instanceLimits = Prelude.Nothing,
      storageTypes = Prelude.Nothing,
      additionalLimits = Prelude.Nothing
    }

-- | The limits for a given instance type.
limits_instanceLimits :: Lens.Lens' Limits (Prelude.Maybe InstanceLimits)
limits_instanceLimits = Lens.lens (\Limits' {instanceLimits} -> instanceLimits) (\s@Limits' {} a -> s {instanceLimits = a} :: Limits)

-- | Storage-related attributes that are available for a given instance type.
limits_storageTypes :: Lens.Lens' Limits (Prelude.Maybe [StorageType])
limits_storageTypes = Lens.lens (\Limits' {storageTypes} -> storageTypes) (\s@Limits' {} a -> s {storageTypes = a} :: Limits) Prelude.. Lens.mapping Lens.coerced

-- | List of additional limits that are specific to a given instance type for
-- each of its instance roles.
limits_additionalLimits :: Lens.Lens' Limits (Prelude.Maybe [AdditionalLimit])
limits_additionalLimits = Lens.lens (\Limits' {additionalLimits} -> additionalLimits) (\s@Limits' {} a -> s {additionalLimits = a} :: Limits) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Limits where
  parseJSON =
    Core.withObject
      "Limits"
      ( \x ->
          Limits'
            Prelude.<$> (x Core..:? "InstanceLimits")
            Prelude.<*> (x Core..:? "StorageTypes" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AdditionalLimits"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Limits where
  hashWithSalt _salt Limits' {..} =
    _salt `Prelude.hashWithSalt` instanceLimits
      `Prelude.hashWithSalt` storageTypes
      `Prelude.hashWithSalt` additionalLimits

instance Prelude.NFData Limits where
  rnf Limits' {..} =
    Prelude.rnf instanceLimits
      `Prelude.seq` Prelude.rnf storageTypes
      `Prelude.seq` Prelude.rnf additionalLimits
