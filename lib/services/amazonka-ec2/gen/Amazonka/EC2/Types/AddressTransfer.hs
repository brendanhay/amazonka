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
-- Module      : Amazonka.EC2.Types.AddressTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AddressTransfer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AddressTransferStatus
import qualified Amazonka.Prelude as Prelude

-- | Details on the Elastic IP address transfer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-eips.html#transfer-EIPs-intro Transfer Elastic IP addresses>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ 'newAddressTransfer' smart constructor.
data AddressTransfer = AddressTransfer'
  { -- | The Elastic IP address transfer status.
    addressTransferStatus :: Prelude.Maybe AddressTransferStatus,
    -- | The allocation ID of an Elastic IP address.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The Elastic IP address being transferred.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that you want to transfer the Elastic IP address
    -- to.
    transferAccountId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the Elastic IP address transfer was accepted.
    transferOfferAcceptedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The timestamp when the Elastic IP address transfer expired. When the
    -- source account starts the transfer, the transfer account has seven hours
    -- to allocate the Elastic IP address to complete the transfer, or the
    -- Elastic IP address will return to its original owner.
    transferOfferExpirationTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddressTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressTransferStatus', 'addressTransfer_addressTransferStatus' - The Elastic IP address transfer status.
--
-- 'allocationId', 'addressTransfer_allocationId' - The allocation ID of an Elastic IP address.
--
-- 'publicIp', 'addressTransfer_publicIp' - The Elastic IP address being transferred.
--
-- 'transferAccountId', 'addressTransfer_transferAccountId' - The ID of the account that you want to transfer the Elastic IP address
-- to.
--
-- 'transferOfferAcceptedTimestamp', 'addressTransfer_transferOfferAcceptedTimestamp' - The timestamp when the Elastic IP address transfer was accepted.
--
-- 'transferOfferExpirationTimestamp', 'addressTransfer_transferOfferExpirationTimestamp' - The timestamp when the Elastic IP address transfer expired. When the
-- source account starts the transfer, the transfer account has seven hours
-- to allocate the Elastic IP address to complete the transfer, or the
-- Elastic IP address will return to its original owner.
newAddressTransfer ::
  AddressTransfer
newAddressTransfer =
  AddressTransfer'
    { addressTransferStatus =
        Prelude.Nothing,
      allocationId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      transferAccountId = Prelude.Nothing,
      transferOfferAcceptedTimestamp = Prelude.Nothing,
      transferOfferExpirationTimestamp = Prelude.Nothing
    }

-- | The Elastic IP address transfer status.
addressTransfer_addressTransferStatus :: Lens.Lens' AddressTransfer (Prelude.Maybe AddressTransferStatus)
addressTransfer_addressTransferStatus = Lens.lens (\AddressTransfer' {addressTransferStatus} -> addressTransferStatus) (\s@AddressTransfer' {} a -> s {addressTransferStatus = a} :: AddressTransfer)

-- | The allocation ID of an Elastic IP address.
addressTransfer_allocationId :: Lens.Lens' AddressTransfer (Prelude.Maybe Prelude.Text)
addressTransfer_allocationId = Lens.lens (\AddressTransfer' {allocationId} -> allocationId) (\s@AddressTransfer' {} a -> s {allocationId = a} :: AddressTransfer)

-- | The Elastic IP address being transferred.
addressTransfer_publicIp :: Lens.Lens' AddressTransfer (Prelude.Maybe Prelude.Text)
addressTransfer_publicIp = Lens.lens (\AddressTransfer' {publicIp} -> publicIp) (\s@AddressTransfer' {} a -> s {publicIp = a} :: AddressTransfer)

-- | The ID of the account that you want to transfer the Elastic IP address
-- to.
addressTransfer_transferAccountId :: Lens.Lens' AddressTransfer (Prelude.Maybe Prelude.Text)
addressTransfer_transferAccountId = Lens.lens (\AddressTransfer' {transferAccountId} -> transferAccountId) (\s@AddressTransfer' {} a -> s {transferAccountId = a} :: AddressTransfer)

-- | The timestamp when the Elastic IP address transfer was accepted.
addressTransfer_transferOfferAcceptedTimestamp :: Lens.Lens' AddressTransfer (Prelude.Maybe Prelude.UTCTime)
addressTransfer_transferOfferAcceptedTimestamp = Lens.lens (\AddressTransfer' {transferOfferAcceptedTimestamp} -> transferOfferAcceptedTimestamp) (\s@AddressTransfer' {} a -> s {transferOfferAcceptedTimestamp = a} :: AddressTransfer) Prelude.. Lens.mapping Data._Time

-- | The timestamp when the Elastic IP address transfer expired. When the
-- source account starts the transfer, the transfer account has seven hours
-- to allocate the Elastic IP address to complete the transfer, or the
-- Elastic IP address will return to its original owner.
addressTransfer_transferOfferExpirationTimestamp :: Lens.Lens' AddressTransfer (Prelude.Maybe Prelude.UTCTime)
addressTransfer_transferOfferExpirationTimestamp = Lens.lens (\AddressTransfer' {transferOfferExpirationTimestamp} -> transferOfferExpirationTimestamp) (\s@AddressTransfer' {} a -> s {transferOfferExpirationTimestamp = a} :: AddressTransfer) Prelude.. Lens.mapping Data._Time

instance Data.FromXML AddressTransfer where
  parseXML x =
    AddressTransfer'
      Prelude.<$> (x Data..@? "addressTransferStatus")
      Prelude.<*> (x Data..@? "allocationId")
      Prelude.<*> (x Data..@? "publicIp")
      Prelude.<*> (x Data..@? "transferAccountId")
      Prelude.<*> (x Data..@? "transferOfferAcceptedTimestamp")
      Prelude.<*> (x Data..@? "transferOfferExpirationTimestamp")

instance Prelude.Hashable AddressTransfer where
  hashWithSalt _salt AddressTransfer' {..} =
    _salt
      `Prelude.hashWithSalt` addressTransferStatus
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` transferAccountId
      `Prelude.hashWithSalt` transferOfferAcceptedTimestamp
      `Prelude.hashWithSalt` transferOfferExpirationTimestamp

instance Prelude.NFData AddressTransfer where
  rnf AddressTransfer' {..} =
    Prelude.rnf addressTransferStatus
      `Prelude.seq` Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf transferAccountId
      `Prelude.seq` Prelude.rnf transferOfferAcceptedTimestamp
      `Prelude.seq` Prelude.rnf transferOfferExpirationTimestamp
