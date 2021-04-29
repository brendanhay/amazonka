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
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcPeeringConnectionStateReason where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the status of a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnectionStateReason' smart constructor.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason'
  { -- | A message that provides more information about the status, if
    -- applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC peering connection.
    code :: Prelude.Maybe VpcPeeringConnectionStateReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionStateReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'vpcPeeringConnectionStateReason_message' - A message that provides more information about the status, if
-- applicable.
--
-- 'code', 'vpcPeeringConnectionStateReason_code' - The status of the VPC peering connection.
newVpcPeeringConnectionStateReason ::
  VpcPeeringConnectionStateReason
newVpcPeeringConnectionStateReason =
  VpcPeeringConnectionStateReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message that provides more information about the status, if
-- applicable.
vpcPeeringConnectionStateReason_message :: Lens.Lens' VpcPeeringConnectionStateReason (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionStateReason_message = Lens.lens (\VpcPeeringConnectionStateReason' {message} -> message) (\s@VpcPeeringConnectionStateReason' {} a -> s {message = a} :: VpcPeeringConnectionStateReason)

-- | The status of the VPC peering connection.
vpcPeeringConnectionStateReason_code :: Lens.Lens' VpcPeeringConnectionStateReason (Prelude.Maybe VpcPeeringConnectionStateReasonCode)
vpcPeeringConnectionStateReason_code = Lens.lens (\VpcPeeringConnectionStateReason' {code} -> code) (\s@VpcPeeringConnectionStateReason' {} a -> s {code = a} :: VpcPeeringConnectionStateReason)

instance
  Prelude.FromXML
    VpcPeeringConnectionStateReason
  where
  parseXML x =
    VpcPeeringConnectionStateReason'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance
  Prelude.Hashable
    VpcPeeringConnectionStateReason

instance
  Prelude.NFData
    VpcPeeringConnectionStateReason
