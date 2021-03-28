{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResourceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.ResourceDetails
  ( ResourceDetails (..)
  -- * Smart constructor
  , mkResourceDetails
  -- * Lenses
  , rdEC2ResourceDetails
  ) where

import qualified Network.AWS.CostExplorer.Types.EC2ResourceDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on the resource.
--
-- /See:/ 'mkResourceDetails' smart constructor.
newtype ResourceDetails = ResourceDetails'
  { eC2ResourceDetails :: Core.Maybe Types.EC2ResourceDetails
    -- ^ Details on the Amazon EC2 resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDetails' value with any optional fields omitted.
mkResourceDetails
    :: ResourceDetails
mkResourceDetails
  = ResourceDetails'{eC2ResourceDetails = Core.Nothing}

-- | Details on the Amazon EC2 resource.
--
-- /Note:/ Consider using 'eC2ResourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdEC2ResourceDetails :: Lens.Lens' ResourceDetails (Core.Maybe Types.EC2ResourceDetails)
rdEC2ResourceDetails = Lens.field @"eC2ResourceDetails"
{-# INLINEABLE rdEC2ResourceDetails #-}
{-# DEPRECATED eC2ResourceDetails "Use generic-lens or generic-optics with 'eC2ResourceDetails' instead"  #-}

instance Core.FromJSON ResourceDetails where
        parseJSON
          = Core.withObject "ResourceDetails" Core.$
              \ x -> ResourceDetails' Core.<$> (x Core..:? "EC2ResourceDetails")
