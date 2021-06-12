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
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcCidrBlockState where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VpcCidrBlockStateCode
import qualified Network.AWS.Lens as Lens

-- | Describes the state of a CIDR block.
--
-- /See:/ 'newVpcCidrBlockState' smart constructor.
data VpcCidrBlockState = VpcCidrBlockState'
  { -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Core.Maybe Core.Text,
    -- | The state of the CIDR block.
    state :: Core.Maybe VpcCidrBlockStateCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcCidrBlockState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'vpcCidrBlockState_statusMessage' - A message about the status of the CIDR block, if applicable.
--
-- 'state', 'vpcCidrBlockState_state' - The state of the CIDR block.
newVpcCidrBlockState ::
  VpcCidrBlockState
newVpcCidrBlockState =
  VpcCidrBlockState'
    { statusMessage = Core.Nothing,
      state = Core.Nothing
    }

-- | A message about the status of the CIDR block, if applicable.
vpcCidrBlockState_statusMessage :: Lens.Lens' VpcCidrBlockState (Core.Maybe Core.Text)
vpcCidrBlockState_statusMessage = Lens.lens (\VpcCidrBlockState' {statusMessage} -> statusMessage) (\s@VpcCidrBlockState' {} a -> s {statusMessage = a} :: VpcCidrBlockState)

-- | The state of the CIDR block.
vpcCidrBlockState_state :: Lens.Lens' VpcCidrBlockState (Core.Maybe VpcCidrBlockStateCode)
vpcCidrBlockState_state = Lens.lens (\VpcCidrBlockState' {state} -> state) (\s@VpcCidrBlockState' {} a -> s {state = a} :: VpcCidrBlockState)

instance Core.FromXML VpcCidrBlockState where
  parseXML x =
    VpcCidrBlockState'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "state")

instance Core.Hashable VpcCidrBlockState

instance Core.NFData VpcCidrBlockState
