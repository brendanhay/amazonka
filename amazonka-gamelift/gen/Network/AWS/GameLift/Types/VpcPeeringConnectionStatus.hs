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
-- Module      : Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VpcPeeringConnectionStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents status information for a VPC peering connection. Status is
-- associated with a VpcPeeringConnection object. Status codes and messages
-- are provided from EC2 (see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html VpcPeeringConnectionStateReason>).
-- Connection status information is also communicated as a fleet Event.
--
-- /See:/ 'newVpcPeeringConnectionStatus' smart constructor.
data VpcPeeringConnectionStatus = VpcPeeringConnectionStatus'
  { -- | Additional messaging associated with the connection status.
    message :: Prelude.Maybe Prelude.Text,
    -- | Code indicating the status of a VPC peering connection.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'vpcPeeringConnectionStatus_message' - Additional messaging associated with the connection status.
--
-- 'code', 'vpcPeeringConnectionStatus_code' - Code indicating the status of a VPC peering connection.
newVpcPeeringConnectionStatus ::
  VpcPeeringConnectionStatus
newVpcPeeringConnectionStatus =
  VpcPeeringConnectionStatus'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | Additional messaging associated with the connection status.
vpcPeeringConnectionStatus_message :: Lens.Lens' VpcPeeringConnectionStatus (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionStatus_message = Lens.lens (\VpcPeeringConnectionStatus' {message} -> message) (\s@VpcPeeringConnectionStatus' {} a -> s {message = a} :: VpcPeeringConnectionStatus)

-- | Code indicating the status of a VPC peering connection.
vpcPeeringConnectionStatus_code :: Lens.Lens' VpcPeeringConnectionStatus (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionStatus_code = Lens.lens (\VpcPeeringConnectionStatus' {code} -> code) (\s@VpcPeeringConnectionStatus' {} a -> s {code = a} :: VpcPeeringConnectionStatus)

instance Prelude.FromJSON VpcPeeringConnectionStatus where
  parseJSON =
    Prelude.withObject
      "VpcPeeringConnectionStatus"
      ( \x ->
          VpcPeeringConnectionStatus'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Code")
      )

instance Prelude.Hashable VpcPeeringConnectionStatus

instance Prelude.NFData VpcPeeringConnectionStatus
