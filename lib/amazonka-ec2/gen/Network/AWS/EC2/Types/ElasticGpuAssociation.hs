{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuAssociation
  ( ElasticGpuAssociation (..),

    -- * Smart constructor
    mkElasticGpuAssociation,

    -- * Lenses
    egaElasticGpuId,
    egaElasticGpuAssociationId,
    egaElasticGpuAssociationTime,
    egaElasticGpuAssociationState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the association between an instance and an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpuAssociation' smart constructor.
data ElasticGpuAssociation = ElasticGpuAssociation'
  { elasticGpuId ::
      Lude.Maybe Lude.Text,
    elasticGpuAssociationId :: Lude.Maybe Lude.Text,
    elasticGpuAssociationTime ::
      Lude.Maybe Lude.Text,
    elasticGpuAssociationState ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticGpuAssociation' with the minimum fields required to make a request.
--
-- * 'elasticGpuAssociationId' - The ID of the association.
-- * 'elasticGpuAssociationState' - The state of the association between the instance and the Elastic Graphics accelerator.
-- * 'elasticGpuAssociationTime' - The time the Elastic Graphics accelerator was associated with the instance.
-- * 'elasticGpuId' - The ID of the Elastic Graphics accelerator.
mkElasticGpuAssociation ::
  ElasticGpuAssociation
mkElasticGpuAssociation =
  ElasticGpuAssociation'
    { elasticGpuId = Lude.Nothing,
      elasticGpuAssociationId = Lude.Nothing,
      elasticGpuAssociationTime = Lude.Nothing,
      elasticGpuAssociationState = Lude.Nothing
    }

-- | The ID of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuId :: Lens.Lens' ElasticGpuAssociation (Lude.Maybe Lude.Text)
egaElasticGpuId = Lens.lens (elasticGpuId :: ElasticGpuAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticGpuId = a} :: ElasticGpuAssociation)
{-# DEPRECATED egaElasticGpuId "Use generic-lens or generic-optics with 'elasticGpuId' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'elasticGpuAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuAssociationId :: Lens.Lens' ElasticGpuAssociation (Lude.Maybe Lude.Text)
egaElasticGpuAssociationId = Lens.lens (elasticGpuAssociationId :: ElasticGpuAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticGpuAssociationId = a} :: ElasticGpuAssociation)
{-# DEPRECATED egaElasticGpuAssociationId "Use generic-lens or generic-optics with 'elasticGpuAssociationId' instead." #-}

-- | The time the Elastic Graphics accelerator was associated with the instance.
--
-- /Note:/ Consider using 'elasticGpuAssociationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuAssociationTime :: Lens.Lens' ElasticGpuAssociation (Lude.Maybe Lude.Text)
egaElasticGpuAssociationTime = Lens.lens (elasticGpuAssociationTime :: ElasticGpuAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticGpuAssociationTime = a} :: ElasticGpuAssociation)
{-# DEPRECATED egaElasticGpuAssociationTime "Use generic-lens or generic-optics with 'elasticGpuAssociationTime' instead." #-}

-- | The state of the association between the instance and the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuAssociationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egaElasticGpuAssociationState :: Lens.Lens' ElasticGpuAssociation (Lude.Maybe Lude.Text)
egaElasticGpuAssociationState = Lens.lens (elasticGpuAssociationState :: ElasticGpuAssociation -> Lude.Maybe Lude.Text) (\s a -> s {elasticGpuAssociationState = a} :: ElasticGpuAssociation)
{-# DEPRECATED egaElasticGpuAssociationState "Use generic-lens or generic-optics with 'elasticGpuAssociationState' instead." #-}

instance Lude.FromXML ElasticGpuAssociation where
  parseXML x =
    ElasticGpuAssociation'
      Lude.<$> (x Lude..@? "elasticGpuId")
      Lude.<*> (x Lude..@? "elasticGpuAssociationId")
      Lude.<*> (x Lude..@? "elasticGpuAssociationTime")
      Lude.<*> (x Lude..@? "elasticGpuAssociationState")
