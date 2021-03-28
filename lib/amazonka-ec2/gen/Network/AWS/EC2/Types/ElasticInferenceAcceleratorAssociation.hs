{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
  ( ElasticInferenceAcceleratorAssociation (..)
  -- * Smart constructor
  , mkElasticInferenceAcceleratorAssociation
  -- * Lenses
  , eiaaElasticInferenceAcceleratorArn
  , eiaaElasticInferenceAcceleratorAssociationId
  , eiaaElasticInferenceAcceleratorAssociationState
  , eiaaElasticInferenceAcceleratorAssociationTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the association between an instance and an elastic inference accelerator. 
--
-- /See:/ 'mkElasticInferenceAcceleratorAssociation' smart constructor.
data ElasticInferenceAcceleratorAssociation = ElasticInferenceAcceleratorAssociation'
  { elasticInferenceAcceleratorArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the elastic inference accelerator. 
  , elasticInferenceAcceleratorAssociationId :: Core.Maybe Core.Text
    -- ^ The ID of the association. 
  , elasticInferenceAcceleratorAssociationState :: Core.Maybe Core.Text
    -- ^ The state of the elastic inference accelerator. 
  , elasticInferenceAcceleratorAssociationTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which the elastic inference accelerator is associated with an instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ElasticInferenceAcceleratorAssociation' value with any optional fields omitted.
mkElasticInferenceAcceleratorAssociation
    :: ElasticInferenceAcceleratorAssociation
mkElasticInferenceAcceleratorAssociation
  = ElasticInferenceAcceleratorAssociation'{elasticInferenceAcceleratorArn
                                              = Core.Nothing,
                                            elasticInferenceAcceleratorAssociationId = Core.Nothing,
                                            elasticInferenceAcceleratorAssociationState =
                                              Core.Nothing,
                                            elasticInferenceAcceleratorAssociationTime =
                                              Core.Nothing}

-- | The Amazon Resource Name (ARN) of the elastic inference accelerator. 
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorArn :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.Text)
eiaaElasticInferenceAcceleratorArn = Lens.field @"elasticInferenceAcceleratorArn"
{-# INLINEABLE eiaaElasticInferenceAcceleratorArn #-}
{-# DEPRECATED elasticInferenceAcceleratorArn "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorArn' instead"  #-}

-- | The ID of the association. 
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationId :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.Text)
eiaaElasticInferenceAcceleratorAssociationId = Lens.field @"elasticInferenceAcceleratorAssociationId"
{-# INLINEABLE eiaaElasticInferenceAcceleratorAssociationId #-}
{-# DEPRECATED elasticInferenceAcceleratorAssociationId "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationId' instead"  #-}

-- | The state of the elastic inference accelerator. 
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationState :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.Text)
eiaaElasticInferenceAcceleratorAssociationState = Lens.field @"elasticInferenceAcceleratorAssociationState"
{-# INLINEABLE eiaaElasticInferenceAcceleratorAssociationState #-}
{-# DEPRECATED elasticInferenceAcceleratorAssociationState "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationState' instead"  #-}

-- | The time at which the elastic inference accelerator is associated with an instance. 
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationTime :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.UTCTime)
eiaaElasticInferenceAcceleratorAssociationTime = Lens.field @"elasticInferenceAcceleratorAssociationTime"
{-# INLINEABLE eiaaElasticInferenceAcceleratorAssociationTime #-}
{-# DEPRECATED elasticInferenceAcceleratorAssociationTime "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationTime' instead"  #-}

instance Core.FromXML ElasticInferenceAcceleratorAssociation where
        parseXML x
          = ElasticInferenceAcceleratorAssociation' Core.<$>
              (x Core..@? "elasticInferenceAcceleratorArn") Core.<*>
                x Core..@? "elasticInferenceAcceleratorAssociationId"
                Core.<*> x Core..@? "elasticInferenceAcceleratorAssociationState"
                Core.<*> x Core..@? "elasticInferenceAcceleratorAssociationTime"
