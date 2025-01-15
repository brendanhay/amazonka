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
-- Module      : Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSUserOrGroupQuota where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSQuotaType
import qualified Amazonka.Prelude as Prelude

-- | The configuration for how much storage a user or group can use on the
-- volume.
--
-- /See:/ 'newOpenZFSUserOrGroupQuota' smart constructor.
data OpenZFSUserOrGroupQuota = OpenZFSUserOrGroupQuota'
  { -- | A value that specifies whether the quota applies to a user or group.
    type' :: OpenZFSQuotaType,
    -- | The ID of the user or group.
    id :: Prelude.Natural,
    -- | The amount of storage that the user or group can use in gibibytes (GiB).
    storageCapacityQuotaGiB :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenZFSUserOrGroupQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'openZFSUserOrGroupQuota_type' - A value that specifies whether the quota applies to a user or group.
--
-- 'id', 'openZFSUserOrGroupQuota_id' - The ID of the user or group.
--
-- 'storageCapacityQuotaGiB', 'openZFSUserOrGroupQuota_storageCapacityQuotaGiB' - The amount of storage that the user or group can use in gibibytes (GiB).
newOpenZFSUserOrGroupQuota ::
  -- | 'type''
  OpenZFSQuotaType ->
  -- | 'id'
  Prelude.Natural ->
  -- | 'storageCapacityQuotaGiB'
  Prelude.Natural ->
  OpenZFSUserOrGroupQuota
newOpenZFSUserOrGroupQuota
  pType_
  pId_
  pStorageCapacityQuotaGiB_ =
    OpenZFSUserOrGroupQuota'
      { type' = pType_,
        id = pId_,
        storageCapacityQuotaGiB =
          pStorageCapacityQuotaGiB_
      }

-- | A value that specifies whether the quota applies to a user or group.
openZFSUserOrGroupQuota_type :: Lens.Lens' OpenZFSUserOrGroupQuota OpenZFSQuotaType
openZFSUserOrGroupQuota_type = Lens.lens (\OpenZFSUserOrGroupQuota' {type'} -> type') (\s@OpenZFSUserOrGroupQuota' {} a -> s {type' = a} :: OpenZFSUserOrGroupQuota)

-- | The ID of the user or group.
openZFSUserOrGroupQuota_id :: Lens.Lens' OpenZFSUserOrGroupQuota Prelude.Natural
openZFSUserOrGroupQuota_id = Lens.lens (\OpenZFSUserOrGroupQuota' {id} -> id) (\s@OpenZFSUserOrGroupQuota' {} a -> s {id = a} :: OpenZFSUserOrGroupQuota)

-- | The amount of storage that the user or group can use in gibibytes (GiB).
openZFSUserOrGroupQuota_storageCapacityQuotaGiB :: Lens.Lens' OpenZFSUserOrGroupQuota Prelude.Natural
openZFSUserOrGroupQuota_storageCapacityQuotaGiB = Lens.lens (\OpenZFSUserOrGroupQuota' {storageCapacityQuotaGiB} -> storageCapacityQuotaGiB) (\s@OpenZFSUserOrGroupQuota' {} a -> s {storageCapacityQuotaGiB = a} :: OpenZFSUserOrGroupQuota)

instance Data.FromJSON OpenZFSUserOrGroupQuota where
  parseJSON =
    Data.withObject
      "OpenZFSUserOrGroupQuota"
      ( \x ->
          OpenZFSUserOrGroupQuota'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "StorageCapacityQuotaGiB")
      )

instance Prelude.Hashable OpenZFSUserOrGroupQuota where
  hashWithSalt _salt OpenZFSUserOrGroupQuota' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` storageCapacityQuotaGiB

instance Prelude.NFData OpenZFSUserOrGroupQuota where
  rnf OpenZFSUserOrGroupQuota' {..} =
    Prelude.rnf type' `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf storageCapacityQuotaGiB

instance Data.ToJSON OpenZFSUserOrGroupQuota where
  toJSON OpenZFSUserOrGroupQuota' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just
              ( "StorageCapacityQuotaGiB"
                  Data..= storageCapacityQuotaGiB
              )
          ]
      )
