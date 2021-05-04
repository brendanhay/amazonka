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
-- Module      : Network.AWS.EC2.Types.MovingAddressStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MovingAddressStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.MoveStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the status of a moving Elastic IP address.
--
-- /See:/ 'newMovingAddressStatus' smart constructor.
data MovingAddressStatus = MovingAddressStatus'
  { -- | The status of the Elastic IP address that\'s being moved to the EC2-VPC
    -- platform, or restored to the EC2-Classic platform.
    moveStatus :: Prelude.Maybe MoveStatus,
    -- | The Elastic IP address.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MovingAddressStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'moveStatus', 'movingAddressStatus_moveStatus' - The status of the Elastic IP address that\'s being moved to the EC2-VPC
-- platform, or restored to the EC2-Classic platform.
--
-- 'publicIp', 'movingAddressStatus_publicIp' - The Elastic IP address.
newMovingAddressStatus ::
  MovingAddressStatus
newMovingAddressStatus =
  MovingAddressStatus'
    { moveStatus = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | The status of the Elastic IP address that\'s being moved to the EC2-VPC
-- platform, or restored to the EC2-Classic platform.
movingAddressStatus_moveStatus :: Lens.Lens' MovingAddressStatus (Prelude.Maybe MoveStatus)
movingAddressStatus_moveStatus = Lens.lens (\MovingAddressStatus' {moveStatus} -> moveStatus) (\s@MovingAddressStatus' {} a -> s {moveStatus = a} :: MovingAddressStatus)

-- | The Elastic IP address.
movingAddressStatus_publicIp :: Lens.Lens' MovingAddressStatus (Prelude.Maybe Prelude.Text)
movingAddressStatus_publicIp = Lens.lens (\MovingAddressStatus' {publicIp} -> publicIp) (\s@MovingAddressStatus' {} a -> s {publicIp = a} :: MovingAddressStatus)

instance Prelude.FromXML MovingAddressStatus where
  parseXML x =
    MovingAddressStatus'
      Prelude.<$> (x Prelude..@? "moveStatus")
      Prelude.<*> (x Prelude..@? "publicIp")

instance Prelude.Hashable MovingAddressStatus

instance Prelude.NFData MovingAddressStatus
