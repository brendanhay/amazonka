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
-- Module      : Amazonka.EC2.Types.MovingAddressStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MovingAddressStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.MoveStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a moving Elastic IP address.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newMovingAddressStatus' smart constructor.
data MovingAddressStatus = MovingAddressStatus'
  { -- | The status of the Elastic IP address that\'s being moved to the EC2-VPC
    -- platform, or restored to the EC2-Classic platform.
    moveStatus :: Prelude.Maybe MoveStatus,
    -- | The Elastic IP address.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML MovingAddressStatus where
  parseXML x =
    MovingAddressStatus'
      Prelude.<$> (x Data..@? "moveStatus")
      Prelude.<*> (x Data..@? "publicIp")

instance Prelude.Hashable MovingAddressStatus where
  hashWithSalt _salt MovingAddressStatus' {..} =
    _salt
      `Prelude.hashWithSalt` moveStatus
      `Prelude.hashWithSalt` publicIp

instance Prelude.NFData MovingAddressStatus where
  rnf MovingAddressStatus' {..} =
    Prelude.rnf moveStatus
      `Prelude.seq` Prelude.rnf publicIp
