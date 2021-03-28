{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ElasticGpuAssociation
  ( ElasticGpuAssociation (..)
  -- * Smart constructor
  , mkElasticGpuAssociation
  -- * Lenses
  , egaElasticGpuAssociationId
  , egaElasticGpuAssociationState
  , egaElasticGpuAssociationTime
  , egaElasticGpuId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the association between an instance and an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpuAssociation' smart constructor.
data ElasticGpuAssociation = ElasticGpuAssociation'
  { elasticGpuAssociationId :: Core.Maybe Core.Text
    -- ^ The ID of the association.
  , elasticGpuAssociationState :: Core.Maybe Core.Text
    -- ^ The state of the association between the instance and the Elastic Graphics accelerator.
  , elasticGpuAssociationTime :: Core.Maybe Core.Text
    -- ^ The time the Elastic Graphics accelerator was associated with the instance.
  , elasticGpuId :: Core.Maybe Core.Text
    -- ^ The ID of the Elastic Graphics accelerator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticGpuAssociation' value with any optional fields omitted.
mkElasticGpuAssociation
    :: ElasticGpuAssociation
mkElasticGpuAssociation
  = ElasticGpuAssociation'{elasticGpuAssociationId = Core.Nothing,
                           elasticGpuAssociationState = Core.Nothing,
                           elasticGpuAssociationTime = Core.Nothing,
                           elasticGpuId = Core.Nothing}

-- | The ID of the association.
--
-- /Note:/ Consider using 'elasticGpuAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuAssociationId :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
egaElasticGpuAssociationId = Lens.field @"elasticGpuAssociationId"
{-# INLINEABLE egaElasticGpuAssociationId #-}
{-# DEPRECATED elasticGpuAssociationId "Use generic-lens or generic-optics with 'elasticGpuAssociationId' instead"  #-}

-- | The state of the association between the instance and the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuAssociationState :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
egaElasticGpuAssociationState = Lens.field @"elasticGpuAssociationState"
{-# INLINEABLE egaElasticGpuAssociationState #-}
{-# DEPRECATED elasticGpuAssociationState "Use generic-lens or generic-optics with 'elasticGpuAssociationState' instead"  #-}

-- | The time the Elastic Graphics accelerator was associated with the instance.
--
-- /Note:/ Consider using 'elasticGpuAssociationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuAssociationTime :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
egaElasticGpuAssociationTime = Lens.field @"elasticGpuAssociationTime"
{-# INLINEABLE egaElasticGpuAssociationTime #-}
{-# DEPRECATED elasticGpuAssociationTime "Use generic-lens or generic-optics with 'elasticGpuAssociationTime' instead"  #-}

-- | The ID of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuId :: Lens.Lens' ElasticGpuAssociation (Core.Maybe Core.Text)
egaElasticGpuId = Lens.field @"elasticGpuId"
{-# INLINEABLE egaElasticGpuId #-}
{-# DEPRECATED elasticGpuId "Use generic-lens or generic-optics with 'elasticGpuId' instead"  #-}

instance Core.FromXML ElasticGpuAssociation where
        parseXML x
          = ElasticGpuAssociation' Core.<$>
              (x Core..@? "elasticGpuAssociationId") Core.<*>
                x Core..@? "elasticGpuAssociationState"
                Core.<*> x Core..@? "elasticGpuAssociationTime"
                Core.<*> x Core..@? "elasticGpuId"
