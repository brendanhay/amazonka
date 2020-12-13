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
    eiaaElasticInferenceAcceleratorAssociationState,
    eiaaElasticInferenceAcceleratorAssociationTime,
    eiaaElasticInferenceAcceleratorARN,
    eiaaElasticInferenceAcceleratorAssociationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the association between an instance and an elastic inference accelerator.
--
-- /See:/ 'mkElasticInferenceAcceleratorAssociation' smart constructor.
data ElasticInferenceAcceleratorAssociation = ElasticInferenceAcceleratorAssociation'
  { -- | The state of the elastic inference accelerator.
    elasticInferenceAcceleratorAssociationState :: Lude.Maybe Lude.Text,
    -- | The time at which the elastic inference accelerator is associated with an instance.
    elasticInferenceAcceleratorAssociationTime :: Lude.Maybe Lude.DateTime,
    -- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
    elasticInferenceAcceleratorARN :: Lude.Maybe Lude.Text,
    -- | The ID of the association.
    elasticInferenceAcceleratorAssociationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticInferenceAcceleratorAssociation' with the minimum fields required to make a request.
--
-- * 'elasticInferenceAcceleratorAssociationState' - The state of the elastic inference accelerator.
-- * 'elasticInferenceAcceleratorAssociationTime' - The time at which the elastic inference accelerator is associated with an instance.
-- * 'elasticInferenceAcceleratorARN' - The Amazon Resource Name (ARN) of the elastic inference accelerator.
-- * 'elasticInferenceAcceleratorAssociationId' - The ID of the association.
mkElasticInferenceAcceleratorAssociation ::
  ElasticInferenceAcceleratorAssociation
mkElasticInferenceAcceleratorAssociation =
  ElasticInferenceAcceleratorAssociation'
    { elasticInferenceAcceleratorAssociationState =
        Lude.Nothing,
      elasticInferenceAcceleratorAssociationTime =
        Lude.Nothing,
      elasticInferenceAcceleratorARN = Lude.Nothing,
      elasticInferenceAcceleratorAssociationId = Lude.Nothing
    }

-- | The state of the elastic inference accelerator.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationState :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Lude.Maybe Lude.Text)
eiaaElasticInferenceAcceleratorAssociationState = Lens.lens (elasticInferenceAcceleratorAssociationState :: ElasticInferenceAcceleratorAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticInferenceAcceleratorAssociationState = a} :: ElasticInferenceAcceleratorAssociation)
{-# DEPRECATED eiaaElasticInferenceAcceleratorAssociationState "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationState' instead." #-}

-- | The time at which the elastic inference accelerator is associated with an instance.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationTime :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Lude.Maybe Lude.DateTime)
eiaaElasticInferenceAcceleratorAssociationTime = Lens.lens (elasticInferenceAcceleratorAssociationTime :: ElasticInferenceAcceleratorAssociation -> Lude.Maybe Lude.DateTime) (\s a -> s {elasticInferenceAcceleratorAssociationTime = a} :: ElasticInferenceAcceleratorAssociation)
{-# DEPRECATED eiaaElasticInferenceAcceleratorAssociationTime "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorARN :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Lude.Maybe Lude.Text)
eiaaElasticInferenceAcceleratorARN = Lens.lens (elasticInferenceAcceleratorARN :: ElasticInferenceAcceleratorAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticInferenceAcceleratorARN = a} :: ElasticInferenceAcceleratorAssociation)
{-# DEPRECATED eiaaElasticInferenceAcceleratorARN "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorARN' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaaElasticInferenceAcceleratorAssociationId :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Lude.Maybe Lude.Text)
eiaaElasticInferenceAcceleratorAssociationId = Lens.lens (elasticInferenceAcceleratorAssociationId :: ElasticInferenceAcceleratorAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticInferenceAcceleratorAssociationId = a} :: ElasticInferenceAcceleratorAssociation)
{-# DEPRECATED eiaaElasticInferenceAcceleratorAssociationId "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociationId' instead." #-}

instance Lude.FromXML ElasticInferenceAcceleratorAssociation where
  parseXML x =
    ElasticInferenceAcceleratorAssociation'
      Lude.<$> (x Lude..@? "elasticInferenceAcceleratorAssociationState")
      Lude.<*> (x Lude..@? "elasticInferenceAcceleratorAssociationTime")
      Lude.<*> (x Lude..@? "elasticInferenceAcceleratorArn")
      Lude.<*> (x Lude..@? "elasticInferenceAcceleratorAssociationId")
