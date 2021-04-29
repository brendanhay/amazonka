{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticSearch.Types.Limits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.Limits where

import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.StorageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Limits for given InstanceType and for each of it\'s role.
-- Limits contains following @ StorageTypes, @ @ InstanceLimits @ and
-- @ AdditionalLimits @
--
-- /See:/ 'newLimits' smart constructor.
data Limits = Limits'
  { instanceLimits :: Prelude.Maybe InstanceLimits,
    -- | List of additional limits that are specific to a given InstanceType and
    -- for each of it\'s @ InstanceRole @ .
    additionalLimits :: Prelude.Maybe [AdditionalLimit],
    -- | StorageType represents the list of storage related types and attributes
    -- that are available for given InstanceType.
    storageTypes :: Prelude.Maybe [StorageType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Limits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceLimits', 'limits_instanceLimits' - Undocumented member.
--
-- 'additionalLimits', 'limits_additionalLimits' - List of additional limits that are specific to a given InstanceType and
-- for each of it\'s @ InstanceRole @ .
--
-- 'storageTypes', 'limits_storageTypes' - StorageType represents the list of storage related types and attributes
-- that are available for given InstanceType.
newLimits ::
  Limits
newLimits =
  Limits'
    { instanceLimits = Prelude.Nothing,
      additionalLimits = Prelude.Nothing,
      storageTypes = Prelude.Nothing
    }

-- | Undocumented member.
limits_instanceLimits :: Lens.Lens' Limits (Prelude.Maybe InstanceLimits)
limits_instanceLimits = Lens.lens (\Limits' {instanceLimits} -> instanceLimits) (\s@Limits' {} a -> s {instanceLimits = a} :: Limits)

-- | List of additional limits that are specific to a given InstanceType and
-- for each of it\'s @ InstanceRole @ .
limits_additionalLimits :: Lens.Lens' Limits (Prelude.Maybe [AdditionalLimit])
limits_additionalLimits = Lens.lens (\Limits' {additionalLimits} -> additionalLimits) (\s@Limits' {} a -> s {additionalLimits = a} :: Limits) Prelude.. Lens.mapping Prelude._Coerce

-- | StorageType represents the list of storage related types and attributes
-- that are available for given InstanceType.
limits_storageTypes :: Lens.Lens' Limits (Prelude.Maybe [StorageType])
limits_storageTypes = Lens.lens (\Limits' {storageTypes} -> storageTypes) (\s@Limits' {} a -> s {storageTypes = a} :: Limits) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Limits where
  parseJSON =
    Prelude.withObject
      "Limits"
      ( \x ->
          Limits'
            Prelude.<$> (x Prelude..:? "InstanceLimits")
            Prelude.<*> ( x Prelude..:? "AdditionalLimits"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "StorageTypes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Limits

instance Prelude.NFData Limits
