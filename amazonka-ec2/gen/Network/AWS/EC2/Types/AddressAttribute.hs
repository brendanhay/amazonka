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
-- Module      : Network.AWS.EC2.Types.AddressAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddressAttribute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PtrUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The attributes associated with an Elastic IP address.
--
-- /See:/ 'newAddressAttribute' smart constructor.
data AddressAttribute = AddressAttribute'
  { -- | The pointer (PTR) record for the IP address.
    ptrRecord :: Prelude.Maybe Prelude.Text,
    -- | The public IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | [EC2-VPC] The allocation ID.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The updated PTR record for the IP address.
    ptrRecordUpdate :: Prelude.Maybe PtrUpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddressAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ptrRecord', 'addressAttribute_ptrRecord' - The pointer (PTR) record for the IP address.
--
-- 'publicIp', 'addressAttribute_publicIp' - The public IP address.
--
-- 'allocationId', 'addressAttribute_allocationId' - [EC2-VPC] The allocation ID.
--
-- 'ptrRecordUpdate', 'addressAttribute_ptrRecordUpdate' - The updated PTR record for the IP address.
newAddressAttribute ::
  AddressAttribute
newAddressAttribute =
  AddressAttribute'
    { ptrRecord = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      ptrRecordUpdate = Prelude.Nothing
    }

-- | The pointer (PTR) record for the IP address.
addressAttribute_ptrRecord :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_ptrRecord = Lens.lens (\AddressAttribute' {ptrRecord} -> ptrRecord) (\s@AddressAttribute' {} a -> s {ptrRecord = a} :: AddressAttribute)

-- | The public IP address.
addressAttribute_publicIp :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_publicIp = Lens.lens (\AddressAttribute' {publicIp} -> publicIp) (\s@AddressAttribute' {} a -> s {publicIp = a} :: AddressAttribute)

-- | [EC2-VPC] The allocation ID.
addressAttribute_allocationId :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_allocationId = Lens.lens (\AddressAttribute' {allocationId} -> allocationId) (\s@AddressAttribute' {} a -> s {allocationId = a} :: AddressAttribute)

-- | The updated PTR record for the IP address.
addressAttribute_ptrRecordUpdate :: Lens.Lens' AddressAttribute (Prelude.Maybe PtrUpdateStatus)
addressAttribute_ptrRecordUpdate = Lens.lens (\AddressAttribute' {ptrRecordUpdate} -> ptrRecordUpdate) (\s@AddressAttribute' {} a -> s {ptrRecordUpdate = a} :: AddressAttribute)

instance Prelude.FromXML AddressAttribute where
  parseXML x =
    AddressAttribute'
      Prelude.<$> (x Prelude..@? "ptrRecord")
      Prelude.<*> (x Prelude..@? "publicIp")
      Prelude.<*> (x Prelude..@? "allocationId")
      Prelude.<*> (x Prelude..@? "ptrRecordUpdate")

instance Prelude.Hashable AddressAttribute

instance Prelude.NFData AddressAttribute
