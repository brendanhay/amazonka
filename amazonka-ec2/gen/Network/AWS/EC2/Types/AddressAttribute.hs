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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PtrUpdateStatus
import qualified Network.AWS.Lens as Lens

-- | The attributes associated with an Elastic IP address.
--
-- /See:/ 'newAddressAttribute' smart constructor.
data AddressAttribute = AddressAttribute'
  { -- | The pointer (PTR) record for the IP address.
    ptrRecord :: Core.Maybe Core.Text,
    -- | The public IP address.
    publicIp :: Core.Maybe Core.Text,
    -- | [EC2-VPC] The allocation ID.
    allocationId :: Core.Maybe Core.Text,
    -- | The updated PTR record for the IP address.
    ptrRecordUpdate :: Core.Maybe PtrUpdateStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ptrRecord = Core.Nothing,
      publicIp = Core.Nothing,
      allocationId = Core.Nothing,
      ptrRecordUpdate = Core.Nothing
    }

-- | The pointer (PTR) record for the IP address.
addressAttribute_ptrRecord :: Lens.Lens' AddressAttribute (Core.Maybe Core.Text)
addressAttribute_ptrRecord = Lens.lens (\AddressAttribute' {ptrRecord} -> ptrRecord) (\s@AddressAttribute' {} a -> s {ptrRecord = a} :: AddressAttribute)

-- | The public IP address.
addressAttribute_publicIp :: Lens.Lens' AddressAttribute (Core.Maybe Core.Text)
addressAttribute_publicIp = Lens.lens (\AddressAttribute' {publicIp} -> publicIp) (\s@AddressAttribute' {} a -> s {publicIp = a} :: AddressAttribute)

-- | [EC2-VPC] The allocation ID.
addressAttribute_allocationId :: Lens.Lens' AddressAttribute (Core.Maybe Core.Text)
addressAttribute_allocationId = Lens.lens (\AddressAttribute' {allocationId} -> allocationId) (\s@AddressAttribute' {} a -> s {allocationId = a} :: AddressAttribute)

-- | The updated PTR record for the IP address.
addressAttribute_ptrRecordUpdate :: Lens.Lens' AddressAttribute (Core.Maybe PtrUpdateStatus)
addressAttribute_ptrRecordUpdate = Lens.lens (\AddressAttribute' {ptrRecordUpdate} -> ptrRecordUpdate) (\s@AddressAttribute' {} a -> s {ptrRecordUpdate = a} :: AddressAttribute)

instance Core.FromXML AddressAttribute where
  parseXML x =
    AddressAttribute'
      Core.<$> (x Core..@? "ptrRecord")
      Core.<*> (x Core..@? "publicIp")
      Core.<*> (x Core..@? "allocationId")
      Core.<*> (x Core..@? "ptrRecordUpdate")

instance Core.Hashable AddressAttribute

instance Core.NFData AddressAttribute
