{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateDnsDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrivateDnsDetails
  ( PrivateDnsDetails (..)
  -- * Smart constructor
  , mkPrivateDnsDetails
  -- * Lenses
  , pddPrivateDnsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Private DNS name for interface endpoints.
--
-- /See:/ 'mkPrivateDnsDetails' smart constructor.
newtype PrivateDnsDetails = PrivateDnsDetails'
  { privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name assigned to the VPC endpoint service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PrivateDnsDetails' value with any optional fields omitted.
mkPrivateDnsDetails
    :: PrivateDnsDetails
mkPrivateDnsDetails
  = PrivateDnsDetails'{privateDnsName = Core.Nothing}

-- | The private DNS name assigned to the VPC endpoint service.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pddPrivateDnsName :: Lens.Lens' PrivateDnsDetails (Core.Maybe Core.Text)
pddPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE pddPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

instance Core.FromXML PrivateDnsDetails where
        parseXML x
          = PrivateDnsDetails' Core.<$> (x Core..@? "privateDnsName")
