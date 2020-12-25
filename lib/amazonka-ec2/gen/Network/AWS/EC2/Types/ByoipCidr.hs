{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ByoipCidr
  ( ByoipCidr (..),

    -- * Smart constructor
    mkByoipCidr,

    -- * Lenses
    bcCidr,
    bcDescription,
    bcState,
    bcStatusMessage,
  )
where

import qualified Network.AWS.EC2.Types.ByoipCidrState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an address range that is provisioned for use with your AWS resources through bring your own IP addresses (BYOIP).
--
-- /See:/ 'mkByoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { -- | The address range, in CIDR notation.
    cidr :: Core.Maybe Types.String,
    -- | The description of the address range.
    description :: Core.Maybe Types.String,
    -- | The state of the address pool.
    state :: Core.Maybe Types.ByoipCidrState,
    -- | Upon success, contains the ID of the address pool. Otherwise, contains an error message.
    statusMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ByoipCidr' value with any optional fields omitted.
mkByoipCidr ::
  ByoipCidr
mkByoipCidr =
  ByoipCidr'
    { cidr = Core.Nothing,
      description = Core.Nothing,
      state = Core.Nothing,
      statusMessage = Core.Nothing
    }

-- | The address range, in CIDR notation.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCidr :: Lens.Lens' ByoipCidr (Core.Maybe Types.String)
bcCidr = Lens.field @"cidr"
{-# DEPRECATED bcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | The description of the address range.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcDescription :: Lens.Lens' ByoipCidr (Core.Maybe Types.String)
bcDescription = Lens.field @"description"
{-# DEPRECATED bcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The state of the address pool.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcState :: Lens.Lens' ByoipCidr (Core.Maybe Types.ByoipCidrState)
bcState = Lens.field @"state"
{-# DEPRECATED bcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Upon success, contains the ID of the address pool. Otherwise, contains an error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcStatusMessage :: Lens.Lens' ByoipCidr (Core.Maybe Types.String)
bcStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED bcStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Core.FromXML ByoipCidr where
  parseXML x =
    ByoipCidr'
      Core.<$> (x Core..@? "cidr")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "statusMessage")
