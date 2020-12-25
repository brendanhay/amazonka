{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockState
  ( SubnetCidrBlockState (..),

    -- * Smart constructor
    mkSubnetCidrBlockState,

    -- * Lenses
    scbsState,
    scbsStatusMessage,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetCidrBlockStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a CIDR block.
--
-- /See:/ 'mkSubnetCidrBlockState' smart constructor.
data SubnetCidrBlockState = SubnetCidrBlockState'
  { -- | The state of a CIDR block.
    state :: Core.Maybe Types.SubnetCidrBlockStateCode,
    -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetCidrBlockState' value with any optional fields omitted.
mkSubnetCidrBlockState ::
  SubnetCidrBlockState
mkSubnetCidrBlockState =
  SubnetCidrBlockState'
    { state = Core.Nothing,
      statusMessage = Core.Nothing
    }

-- | The state of a CIDR block.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scbsState :: Lens.Lens' SubnetCidrBlockState (Core.Maybe Types.SubnetCidrBlockStateCode)
scbsState = Lens.field @"state"
{-# DEPRECATED scbsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A message about the status of the CIDR block, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scbsStatusMessage :: Lens.Lens' SubnetCidrBlockState (Core.Maybe Types.String)
scbsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED scbsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Core.FromXML SubnetCidrBlockState where
  parseXML x =
    SubnetCidrBlockState'
      Core.<$> (x Core..@? "state") Core.<*> (x Core..@? "statusMessage")
