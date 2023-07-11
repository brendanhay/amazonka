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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.Limits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AdditionalLimit
import Amazonka.OpenSearch.Types.InstanceLimits
import Amazonka.OpenSearch.Types.StorageType
import qualified Amazonka.Prelude as Prelude

-- | Limits for a given instance type and for each of its roles.
--
-- /See:/ 'newLimits' smart constructor.
data Limits = Limits'
  { -- | List of additional limits that are specific to a given instance type for
    -- each of its instance roles.
    additionalLimits :: Prelude.Maybe [AdditionalLimit],
    -- | The limits for a given instance type.
    instanceLimits :: Prelude.Maybe InstanceLimits,
    -- | Storage-related attributes that are available for a given instance type.
    storageTypes :: Prelude.Maybe [StorageType]
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
-- 'additionalLimits', 'limits_additionalLimits' - List of additional limits that are specific to a given instance type for
-- each of its instance roles.
--
-- 'instanceLimits', 'limits_instanceLimits' - The limits for a given instance type.
--
-- 'storageTypes', 'limits_storageTypes' - Storage-related attributes that are available for a given instance type.
newLimits ::
  Limits
newLimits =
  Limits'
    { additionalLimits = Prelude.Nothing,
      instanceLimits = Prelude.Nothing,
      storageTypes = Prelude.Nothing
    }

-- | List of additional limits that are specific to a given instance type for
-- each of its instance roles.
limits_additionalLimits :: Lens.Lens' Limits (Prelude.Maybe [AdditionalLimit])
limits_additionalLimits = Lens.lens (\Limits' {additionalLimits} -> additionalLimits) (\s@Limits' {} a -> s {additionalLimits = a} :: Limits) Prelude.. Lens.mapping Lens.coerced

-- | The limits for a given instance type.
limits_instanceLimits :: Lens.Lens' Limits (Prelude.Maybe InstanceLimits)
limits_instanceLimits = Lens.lens (\Limits' {instanceLimits} -> instanceLimits) (\s@Limits' {} a -> s {instanceLimits = a} :: Limits)

-- | Storage-related attributes that are available for a given instance type.
limits_storageTypes :: Lens.Lens' Limits (Prelude.Maybe [StorageType])
limits_storageTypes = Lens.lens (\Limits' {storageTypes} -> storageTypes) (\s@Limits' {} a -> s {storageTypes = a} :: Limits) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Limits where
  parseJSON =
    Data.withObject
      "Limits"
      ( \x ->
          Limits'
            Prelude.<$> ( x
                            Data..:? "AdditionalLimits"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "InstanceLimits")
            Prelude.<*> (x Data..:? "StorageTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Limits where
  hashWithSalt _salt Limits' {..} =
    _salt
      `Prelude.hashWithSalt` additionalLimits
      `Prelude.hashWithSalt` instanceLimits
      `Prelude.hashWithSalt` storageTypes

instance Prelude.NFData Limits where
  rnf Limits' {..} =
    Prelude.rnf additionalLimits
      `Prelude.seq` Prelude.rnf instanceLimits
      `Prelude.seq` Prelude.rnf storageTypes
