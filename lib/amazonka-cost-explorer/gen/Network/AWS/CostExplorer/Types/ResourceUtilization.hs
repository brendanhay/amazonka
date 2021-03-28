{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.ResourceUtilization
  ( ResourceUtilization (..)
  -- * Smart constructor
  , mkResourceUtilization
  -- * Lenses
  , ruEC2ResourceUtilization
  ) where

import qualified Network.AWS.CostExplorer.Types.EC2ResourceUtilization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Resource utilization of current resource. 
--
-- /See:/ 'mkResourceUtilization' smart constructor.
newtype ResourceUtilization = ResourceUtilization'
  { eC2ResourceUtilization :: Core.Maybe Types.EC2ResourceUtilization
    -- ^ Utilization of current Amazon EC2 instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceUtilization' value with any optional fields omitted.
mkResourceUtilization
    :: ResourceUtilization
mkResourceUtilization
  = ResourceUtilization'{eC2ResourceUtilization = Core.Nothing}

-- | Utilization of current Amazon EC2 instance. 
--
-- /Note:/ Consider using 'eC2ResourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruEC2ResourceUtilization :: Lens.Lens' ResourceUtilization (Core.Maybe Types.EC2ResourceUtilization)
ruEC2ResourceUtilization = Lens.field @"eC2ResourceUtilization"
{-# INLINEABLE ruEC2ResourceUtilization #-}
{-# DEPRECATED eC2ResourceUtilization "Use generic-lens or generic-optics with 'eC2ResourceUtilization' instead"  #-}

instance Core.FromJSON ResourceUtilization where
        parseJSON
          = Core.withObject "ResourceUtilization" Core.$
              \ x ->
                ResourceUtilization' Core.<$> (x Core..:? "EC2ResourceUtilization")
