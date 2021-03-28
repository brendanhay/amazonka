{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ElasticIpStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ElasticIpStatus
  ( ElasticIpStatus (..)
  -- * Smart constructor
  , mkElasticIpStatus
  -- * Lenses
  , eisElasticIp
  , eisStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes the status of the elastic IP (EIP) address.
--
-- /See:/ 'mkElasticIpStatus' smart constructor.
data ElasticIpStatus = ElasticIpStatus'
  { elasticIp :: Core.Maybe Core.Text
    -- ^ The elastic IP (EIP) address for the cluster.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the elastic IP (EIP) address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticIpStatus' value with any optional fields omitted.
mkElasticIpStatus
    :: ElasticIpStatus
mkElasticIpStatus
  = ElasticIpStatus'{elasticIp = Core.Nothing, status = Core.Nothing}

-- | The elastic IP (EIP) address for the cluster.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eisElasticIp :: Lens.Lens' ElasticIpStatus (Core.Maybe Core.Text)
eisElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE eisElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | The status of the elastic IP (EIP) address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eisStatus :: Lens.Lens' ElasticIpStatus (Core.Maybe Core.Text)
eisStatus = Lens.field @"status"
{-# INLINEABLE eisStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML ElasticIpStatus where
        parseXML x
          = ElasticIpStatus' Core.<$>
              (x Core..@? "ElasticIp") Core.<*> x Core..@? "Status"
