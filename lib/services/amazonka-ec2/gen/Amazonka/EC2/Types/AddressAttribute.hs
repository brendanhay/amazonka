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
-- Module      : Amazonka.EC2.Types.AddressAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AddressAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PtrUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | The attributes associated with an Elastic IP address.
--
-- /See:/ 'newAddressAttribute' smart constructor.
data AddressAttribute = AddressAttribute'
  { -- | [EC2-VPC] The allocation ID.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The pointer (PTR) record for the IP address.
    ptrRecord :: Prelude.Maybe Prelude.Text,
    -- | The updated PTR record for the IP address.
    ptrRecordUpdate :: Prelude.Maybe PtrUpdateStatus,
    -- | The public IP address.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddressAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'addressAttribute_allocationId' - [EC2-VPC] The allocation ID.
--
-- 'ptrRecord', 'addressAttribute_ptrRecord' - The pointer (PTR) record for the IP address.
--
-- 'ptrRecordUpdate', 'addressAttribute_ptrRecordUpdate' - The updated PTR record for the IP address.
--
-- 'publicIp', 'addressAttribute_publicIp' - The public IP address.
newAddressAttribute ::
  AddressAttribute
newAddressAttribute =
  AddressAttribute'
    { allocationId = Prelude.Nothing,
      ptrRecord = Prelude.Nothing,
      ptrRecordUpdate = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | [EC2-VPC] The allocation ID.
addressAttribute_allocationId :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_allocationId = Lens.lens (\AddressAttribute' {allocationId} -> allocationId) (\s@AddressAttribute' {} a -> s {allocationId = a} :: AddressAttribute)

-- | The pointer (PTR) record for the IP address.
addressAttribute_ptrRecord :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_ptrRecord = Lens.lens (\AddressAttribute' {ptrRecord} -> ptrRecord) (\s@AddressAttribute' {} a -> s {ptrRecord = a} :: AddressAttribute)

-- | The updated PTR record for the IP address.
addressAttribute_ptrRecordUpdate :: Lens.Lens' AddressAttribute (Prelude.Maybe PtrUpdateStatus)
addressAttribute_ptrRecordUpdate = Lens.lens (\AddressAttribute' {ptrRecordUpdate} -> ptrRecordUpdate) (\s@AddressAttribute' {} a -> s {ptrRecordUpdate = a} :: AddressAttribute)

-- | The public IP address.
addressAttribute_publicIp :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_publicIp = Lens.lens (\AddressAttribute' {publicIp} -> publicIp) (\s@AddressAttribute' {} a -> s {publicIp = a} :: AddressAttribute)

instance Data.FromXML AddressAttribute where
  parseXML x =
    AddressAttribute'
      Prelude.<$> (x Data..@? "allocationId")
      Prelude.<*> (x Data..@? "ptrRecord")
      Prelude.<*> (x Data..@? "ptrRecordUpdate")
      Prelude.<*> (x Data..@? "publicIp")

instance Prelude.Hashable AddressAttribute where
  hashWithSalt _salt AddressAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` ptrRecord
      `Prelude.hashWithSalt` ptrRecordUpdate
      `Prelude.hashWithSalt` publicIp

instance Prelude.NFData AddressAttribute where
  rnf AddressAttribute' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf ptrRecord
      `Prelude.seq` Prelude.rnf ptrRecordUpdate
      `Prelude.seq` Prelude.rnf publicIp
