{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceTypeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ServiceTypeDetail
  ( ServiceTypeDetail (..)
  -- * Smart constructor
  , mkServiceTypeDetail
  -- * Lenses
  , stdServiceType
  ) where

import qualified Network.AWS.EC2.Types.ServiceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the type of service for a VPC endpoint.
--
-- /See:/ 'mkServiceTypeDetail' smart constructor.
newtype ServiceTypeDetail = ServiceTypeDetail'
  { serviceType :: Core.Maybe Types.ServiceType
    -- ^ The type of service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceTypeDetail' value with any optional fields omitted.
mkServiceTypeDetail
    :: ServiceTypeDetail
mkServiceTypeDetail
  = ServiceTypeDetail'{serviceType = Core.Nothing}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdServiceType :: Lens.Lens' ServiceTypeDetail (Core.Maybe Types.ServiceType)
stdServiceType = Lens.field @"serviceType"
{-# INLINEABLE stdServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

instance Core.FromXML ServiceTypeDetail where
        parseXML x = ServiceTypeDetail' Core.<$> (x Core..@? "serviceType")
