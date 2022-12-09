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
-- Module      : Amazonka.GameLift.Types.VpcPeeringConnectionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.VpcPeeringConnectionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents status information for a VPC peering connection. Status codes
-- and messages are provided from EC2 (see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html VpcPeeringConnectionStateReason>).
-- Connection status information is also communicated as a fleet event.
--
-- /See:/ 'newVpcPeeringConnectionStatus' smart constructor.
data VpcPeeringConnectionStatus = VpcPeeringConnectionStatus'
  { -- | Code indicating the status of a VPC peering connection.
    code :: Prelude.Maybe Prelude.Text,
    -- | Additional messaging associated with the connection status.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'vpcPeeringConnectionStatus_code' - Code indicating the status of a VPC peering connection.
--
-- 'message', 'vpcPeeringConnectionStatus_message' - Additional messaging associated with the connection status.
newVpcPeeringConnectionStatus ::
  VpcPeeringConnectionStatus
newVpcPeeringConnectionStatus =
  VpcPeeringConnectionStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Code indicating the status of a VPC peering connection.
vpcPeeringConnectionStatus_code :: Lens.Lens' VpcPeeringConnectionStatus (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionStatus_code = Lens.lens (\VpcPeeringConnectionStatus' {code} -> code) (\s@VpcPeeringConnectionStatus' {} a -> s {code = a} :: VpcPeeringConnectionStatus)

-- | Additional messaging associated with the connection status.
vpcPeeringConnectionStatus_message :: Lens.Lens' VpcPeeringConnectionStatus (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionStatus_message = Lens.lens (\VpcPeeringConnectionStatus' {message} -> message) (\s@VpcPeeringConnectionStatus' {} a -> s {message = a} :: VpcPeeringConnectionStatus)

instance Data.FromJSON VpcPeeringConnectionStatus where
  parseJSON =
    Data.withObject
      "VpcPeeringConnectionStatus"
      ( \x ->
          VpcPeeringConnectionStatus'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable VpcPeeringConnectionStatus where
  hashWithSalt _salt VpcPeeringConnectionStatus' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData VpcPeeringConnectionStatus where
  rnf VpcPeeringConnectionStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
