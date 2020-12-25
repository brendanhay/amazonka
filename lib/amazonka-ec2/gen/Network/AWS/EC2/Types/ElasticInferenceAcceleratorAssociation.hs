{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
  ( ElasticInferenceAcceleratorAssociation (..),

    -- * Smart constructor
    mkElasticInferenceAcceleratorAssociation,

    -- * Lenses
    eiaaElasticInferenceAcceleratorArn,
    eiaaElasticInferenceAcceleratorAssociationId,
    eiaaElasticInferenceAcceleratorAssociationState,
    eiaaElasticInferenceAcceleratorAssociationTime,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the association between an instance and an elastic inference accelerator.
--
-- /See:/ 'mkElasticInferenceAcceleratorAssociation' smart constructor.
data ElasticInferenceAcceleratorAssociation = ElasticInferenceAcceleratorAssociation'
  { -- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
    elasticInferenceAcceleratorArn :: Core.Maybe Types.String,
    -- | The ID of the association.
    elasticInferenceAcceleratorAssociationId :: Core.Maybe Types.String,
    -- | The state of the elastic inference accelerator.
    elasticInferenceAcceleratorAssociationState :: Core.Maybe Types.String,
    -- | The time at which the elastic inference accelerator is associated with an instance.
    elasticInferenceAcceleratorAssociationTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ElasticInferenceAcceleratorAssociation' value with any optional fields omitted.
mkElasticInferenceAcceleratorAssociation ::
  ElasticInferenceAcceleratorAssociation
mkElasticInferenceAcceleratorAssociation =
  ElasticInferenceAcceleratorAssociation'
    { elasticInferenceAcceleratorArn =
        Core.Nothing,
      elasticInferenceAcceleratorAssociationId = Core.Nothing,
      elasticInferenceAcceleratorAssociationState =
        Core.Nothing,
      elasticInferenceAcceleratorAssociationTime =
        Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorArn :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Types.String)
eiaaElasticInferenceAcceleratorArn = Lens.field @"elasticInferenceAcceleratorArn"
{-# DEPRECATED eiaaElasticInferenceAcceleratorArn "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorArn' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationId :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Types.String)
eiaaElasticInferenceAcceleratorAssociationId = Lens.field @"elasticInferenceAcceleratorAssociationId"
{-# DEPRECATED eiaaElasticInferenceAcceleratorAssociationId "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationId' instead." #-}

-- | The state of the elastic inference accelerator.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationState :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Types.String)
eiaaElasticInferenceAcceleratorAssociationState = Lens.field @"elasticInferenceAcceleratorAssociationState"
{-# DEPRECATED eiaaElasticInferenceAcceleratorAssociationState "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationState' instead." #-}

-- | The time at which the elastic inference accelerator is associated with an instance.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationTime :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Core.Maybe Core.UTCTime)
eiaaElasticInferenceAcceleratorAssociationTime = Lens.field @"elasticInferenceAcceleratorAssociationTime"
{-# DEPRECATED eiaaElasticInferenceAcceleratorAssociationTime "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationTime' instead." #-}

instance Core.FromXML ElasticInferenceAcceleratorAssociation where
  parseXML x =
    ElasticInferenceAcceleratorAssociation'
      Core.<$> (x Core..@? "elasticInferenceAcceleratorArn")
      Core.<*> (x Core..@? "elasticInferenceAcceleratorAssociationId")
      Core.<*> (x Core..@? "elasticInferenceAcceleratorAssociationState")
      Core.<*> (x Core..@? "elasticInferenceAcceleratorAssociationTime")
