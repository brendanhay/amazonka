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
-- Module      : Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PrivateIpAddressDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains other private IP address information of the EC2 instance.
--
-- /See:/ 'newPrivateIpAddressDetails' smart constructor.
data PrivateIpAddressDetails = PrivateIpAddressDetails'
  { -- | The private DNS name of the EC2 instance.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the EC2 instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PrivateIpAddressDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateDnsName', 'privateIpAddressDetails_privateDnsName' - The private DNS name of the EC2 instance.
--
-- 'privateIpAddress', 'privateIpAddressDetails_privateIpAddress' - The private IP address of the EC2 instance.
newPrivateIpAddressDetails ::
  PrivateIpAddressDetails
newPrivateIpAddressDetails =
  PrivateIpAddressDetails'
    { privateDnsName =
        Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The private DNS name of the EC2 instance.
privateIpAddressDetails_privateDnsName :: Lens.Lens' PrivateIpAddressDetails (Prelude.Maybe Prelude.Text)
privateIpAddressDetails_privateDnsName = Lens.lens (\PrivateIpAddressDetails' {privateDnsName} -> privateDnsName) (\s@PrivateIpAddressDetails' {} a -> s {privateDnsName = a} :: PrivateIpAddressDetails)

-- | The private IP address of the EC2 instance.
privateIpAddressDetails_privateIpAddress :: Lens.Lens' PrivateIpAddressDetails (Prelude.Maybe Prelude.Text)
privateIpAddressDetails_privateIpAddress = Lens.lens (\PrivateIpAddressDetails' {privateIpAddress} -> privateIpAddress) (\s@PrivateIpAddressDetails' {} a -> s {privateIpAddress = a} :: PrivateIpAddressDetails)

instance Prelude.FromJSON PrivateIpAddressDetails where
  parseJSON =
    Prelude.withObject
      "PrivateIpAddressDetails"
      ( \x ->
          PrivateIpAddressDetails'
            Prelude.<$> (x Prelude..:? "privateDnsName")
            Prelude.<*> (x Prelude..:? "privateIpAddress")
      )

instance Prelude.Hashable PrivateIpAddressDetails

instance Prelude.NFData PrivateIpAddressDetails
