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
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockState where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetCidrBlockStateCode
import qualified Network.AWS.Lens as Lens

-- | Describes the state of a CIDR block.
--
-- /See:/ 'newSubnetCidrBlockState' smart constructor.
data SubnetCidrBlockState = SubnetCidrBlockState'
  { -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Core.Maybe Core.Text,
    -- | The state of a CIDR block.
    state :: Core.Maybe SubnetCidrBlockStateCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubnetCidrBlockState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'subnetCidrBlockState_statusMessage' - A message about the status of the CIDR block, if applicable.
--
-- 'state', 'subnetCidrBlockState_state' - The state of a CIDR block.
newSubnetCidrBlockState ::
  SubnetCidrBlockState
newSubnetCidrBlockState =
  SubnetCidrBlockState'
    { statusMessage = Core.Nothing,
      state = Core.Nothing
    }

-- | A message about the status of the CIDR block, if applicable.
subnetCidrBlockState_statusMessage :: Lens.Lens' SubnetCidrBlockState (Core.Maybe Core.Text)
subnetCidrBlockState_statusMessage = Lens.lens (\SubnetCidrBlockState' {statusMessage} -> statusMessage) (\s@SubnetCidrBlockState' {} a -> s {statusMessage = a} :: SubnetCidrBlockState)

-- | The state of a CIDR block.
subnetCidrBlockState_state :: Lens.Lens' SubnetCidrBlockState (Core.Maybe SubnetCidrBlockStateCode)
subnetCidrBlockState_state = Lens.lens (\SubnetCidrBlockState' {state} -> state) (\s@SubnetCidrBlockState' {} a -> s {state = a} :: SubnetCidrBlockState)

instance Core.FromXML SubnetCidrBlockState where
  parseXML x =
    SubnetCidrBlockState'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "state")

instance Core.Hashable SubnetCidrBlockState

instance Core.NFData SubnetCidrBlockState
