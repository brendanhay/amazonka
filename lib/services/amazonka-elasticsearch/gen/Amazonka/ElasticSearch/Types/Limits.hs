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
-- Module      : Amazonka.ElasticSearch.Types.Limits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.Limits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.AdditionalLimit
import Amazonka.ElasticSearch.Types.InstanceLimits
import Amazonka.ElasticSearch.Types.StorageType
import qualified Amazonka.Prelude as Prelude

-- | Limits for given InstanceType and for each of it\'s role.
-- Limits contains following @ StorageTypes, @ @ InstanceLimits @ and
-- @ AdditionalLimits @
--
-- /See:/ 'newLimits' smart constructor.
data Limits = Limits'
  { -- | List of additional limits that are specific to a given InstanceType and
    -- for each of it\'s @ InstanceRole @ .
    additionalLimits :: Prelude.Maybe [AdditionalLimit],
    instanceLimits :: Prelude.Maybe InstanceLimits,
    -- | StorageType represents the list of storage related types and attributes
    -- that are available for given InstanceType.
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
-- 'additionalLimits', 'limits_additionalLimits' - List of additional limits that are specific to a given InstanceType and
-- for each of it\'s @ InstanceRole @ .
--
-- 'instanceLimits', 'limits_instanceLimits' - Undocumented member.
--
-- 'storageTypes', 'limits_storageTypes' - StorageType represents the list of storage related types and attributes
-- that are available for given InstanceType.
newLimits ::
  Limits
newLimits =
  Limits'
    { additionalLimits = Prelude.Nothing,
      instanceLimits = Prelude.Nothing,
      storageTypes = Prelude.Nothing
    }

-- | List of additional limits that are specific to a given InstanceType and
-- for each of it\'s @ InstanceRole @ .
limits_additionalLimits :: Lens.Lens' Limits (Prelude.Maybe [AdditionalLimit])
limits_additionalLimits = Lens.lens (\Limits' {additionalLimits} -> additionalLimits) (\s@Limits' {} a -> s {additionalLimits = a} :: Limits) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
limits_instanceLimits :: Lens.Lens' Limits (Prelude.Maybe InstanceLimits)
limits_instanceLimits = Lens.lens (\Limits' {instanceLimits} -> instanceLimits) (\s@Limits' {} a -> s {instanceLimits = a} :: Limits)

-- | StorageType represents the list of storage related types and attributes
-- that are available for given InstanceType.
limits_storageTypes :: Lens.Lens' Limits (Prelude.Maybe [StorageType])
limits_storageTypes = Lens.lens (\Limits' {storageTypes} -> storageTypes) (\s@Limits' {} a -> s {storageTypes = a} :: Limits) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Limits where
  parseJSON =
    Data.withObject
      "Limits"
      ( \x ->
          Limits'
            Prelude.<$> ( x Data..:? "AdditionalLimits"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "InstanceLimits")
            Prelude.<*> (x Data..:? "StorageTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Limits where
  hashWithSalt _salt Limits' {..} =
    _salt `Prelude.hashWithSalt` additionalLimits
      `Prelude.hashWithSalt` instanceLimits
      `Prelude.hashWithSalt` storageTypes

instance Prelude.NFData Limits where
  rnf Limits' {..} =
    Prelude.rnf additionalLimits
      `Prelude.seq` Prelude.rnf instanceLimits
      `Prelude.seq` Prelude.rnf storageTypes
