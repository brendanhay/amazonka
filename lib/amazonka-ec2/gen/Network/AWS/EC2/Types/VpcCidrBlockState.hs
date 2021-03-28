{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcCidrBlockState
  ( VpcCidrBlockState (..)
  -- * Smart constructor
  , mkVpcCidrBlockState
  -- * Lenses
  , vcbsState
  , vcbsStatusMessage
  ) where

import qualified Network.AWS.EC2.Types.VpcCidrBlockStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a CIDR block.
--
-- /See:/ 'mkVpcCidrBlockState' smart constructor.
data VpcCidrBlockState = VpcCidrBlockState'
  { state :: Core.Maybe Types.VpcCidrBlockStateCode
    -- ^ The state of the CIDR block.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A message about the status of the CIDR block, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcCidrBlockState' value with any optional fields omitted.
mkVpcCidrBlockState
    :: VpcCidrBlockState
mkVpcCidrBlockState
  = VpcCidrBlockState'{state = Core.Nothing,
                       statusMessage = Core.Nothing}

-- | The state of the CIDR block.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbsState :: Lens.Lens' VpcCidrBlockState (Core.Maybe Types.VpcCidrBlockStateCode)
vcbsState = Lens.field @"state"
{-# INLINEABLE vcbsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A message about the status of the CIDR block, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbsStatusMessage :: Lens.Lens' VpcCidrBlockState (Core.Maybe Core.Text)
vcbsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE vcbsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

instance Core.FromXML VpcCidrBlockState where
        parseXML x
          = VpcCidrBlockState' Core.<$>
              (x Core..@? "state") Core.<*> x Core..@? "statusMessage"
