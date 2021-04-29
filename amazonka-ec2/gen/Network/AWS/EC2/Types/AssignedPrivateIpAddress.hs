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
-- Module      : Network.AWS.EC2.Types.AssignedPrivateIpAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssignedPrivateIpAddress where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the private IP addresses assigned to a network interface.
--
-- /See:/ 'newAssignedPrivateIpAddress' smart constructor.
data AssignedPrivateIpAddress = AssignedPrivateIpAddress'
  { -- | The private IP address assigned to the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignedPrivateIpAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddress', 'assignedPrivateIpAddress_privateIpAddress' - The private IP address assigned to the network interface.
newAssignedPrivateIpAddress ::
  AssignedPrivateIpAddress
newAssignedPrivateIpAddress =
  AssignedPrivateIpAddress'
    { privateIpAddress =
        Prelude.Nothing
    }

-- | The private IP address assigned to the network interface.
assignedPrivateIpAddress_privateIpAddress :: Lens.Lens' AssignedPrivateIpAddress (Prelude.Maybe Prelude.Text)
assignedPrivateIpAddress_privateIpAddress = Lens.lens (\AssignedPrivateIpAddress' {privateIpAddress} -> privateIpAddress) (\s@AssignedPrivateIpAddress' {} a -> s {privateIpAddress = a} :: AssignedPrivateIpAddress)

instance Prelude.FromXML AssignedPrivateIpAddress where
  parseXML x =
    AssignedPrivateIpAddress'
      Prelude.<$> (x Prelude..@? "privateIpAddress")

instance Prelude.Hashable AssignedPrivateIpAddress

instance Prelude.NFData AssignedPrivateIpAddress
