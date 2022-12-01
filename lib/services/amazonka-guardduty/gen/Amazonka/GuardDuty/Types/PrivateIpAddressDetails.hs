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
-- Module      : Amazonka.GuardDuty.Types.PrivateIpAddressDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.PrivateIpAddressDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains other private IP address information of the EC2 instance.
--
-- /See:/ 'newPrivateIpAddressDetails' smart constructor.
data PrivateIpAddressDetails = PrivateIpAddressDetails'
  { -- | The private IP address of the EC2 instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name of the EC2 instance.
    privateDnsName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateIpAddressDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddress', 'privateIpAddressDetails_privateIpAddress' - The private IP address of the EC2 instance.
--
-- 'privateDnsName', 'privateIpAddressDetails_privateDnsName' - The private DNS name of the EC2 instance.
newPrivateIpAddressDetails ::
  PrivateIpAddressDetails
newPrivateIpAddressDetails =
  PrivateIpAddressDetails'
    { privateIpAddress =
        Prelude.Nothing,
      privateDnsName = Prelude.Nothing
    }

-- | The private IP address of the EC2 instance.
privateIpAddressDetails_privateIpAddress :: Lens.Lens' PrivateIpAddressDetails (Prelude.Maybe Prelude.Text)
privateIpAddressDetails_privateIpAddress = Lens.lens (\PrivateIpAddressDetails' {privateIpAddress} -> privateIpAddress) (\s@PrivateIpAddressDetails' {} a -> s {privateIpAddress = a} :: PrivateIpAddressDetails)

-- | The private DNS name of the EC2 instance.
privateIpAddressDetails_privateDnsName :: Lens.Lens' PrivateIpAddressDetails (Prelude.Maybe Prelude.Text)
privateIpAddressDetails_privateDnsName = Lens.lens (\PrivateIpAddressDetails' {privateDnsName} -> privateDnsName) (\s@PrivateIpAddressDetails' {} a -> s {privateDnsName = a} :: PrivateIpAddressDetails)

instance Core.FromJSON PrivateIpAddressDetails where
  parseJSON =
    Core.withObject
      "PrivateIpAddressDetails"
      ( \x ->
          PrivateIpAddressDetails'
            Prelude.<$> (x Core..:? "privateIpAddress")
            Prelude.<*> (x Core..:? "privateDnsName")
      )

instance Prelude.Hashable PrivateIpAddressDetails where
  hashWithSalt _salt PrivateIpAddressDetails' {..} =
    _salt `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` privateDnsName

instance Prelude.NFData PrivateIpAddressDetails where
  rnf PrivateIpAddressDetails' {..} =
    Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf privateDnsName
