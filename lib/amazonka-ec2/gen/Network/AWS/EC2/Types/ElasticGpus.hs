-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpus
  ( ElasticGpus (..),

    -- * Smart constructor
    mkElasticGpus,

    -- * Lenses
    egInstanceId,
    egElasticGpuType,
    egElasticGpuId,
    egElasticGpuState,
    egElasticGpuHealth,
    egAvailabilityZone,
    egTags,
  )
where

import Network.AWS.EC2.Types.ElasticGpuHealth
import Network.AWS.EC2.Types.ElasticGpuState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { instanceId :: Lude.Maybe Lude.Text,
    elasticGpuType :: Lude.Maybe Lude.Text,
    elasticGpuId :: Lude.Maybe Lude.Text,
    elasticGpuState :: Lude.Maybe ElasticGpuState,
    elasticGpuHealth :: Lude.Maybe ElasticGpuHealth,
    availabilityZone :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticGpus' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in the which the Elastic Graphics accelerator resides.
-- * 'elasticGpuHealth' - The status of the Elastic Graphics accelerator.
-- * 'elasticGpuId' - The ID of the Elastic Graphics accelerator.
-- * 'elasticGpuState' - The state of the Elastic Graphics accelerator.
-- * 'elasticGpuType' - The type of Elastic Graphics accelerator.
-- * 'instanceId' - The ID of the instance to which the Elastic Graphics accelerator is attached.
-- * 'tags' - The tags assigned to the Elastic Graphics accelerator.
mkElasticGpus ::
  ElasticGpus
mkElasticGpus =
  ElasticGpus'
    { instanceId = Lude.Nothing,
      elasticGpuType = Lude.Nothing,
      elasticGpuId = Lude.Nothing,
      elasticGpuState = Lude.Nothing,
      elasticGpuHealth = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the instance to which the Elastic Graphics accelerator is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egInstanceId :: Lens.Lens' ElasticGpus (Lude.Maybe Lude.Text)
egInstanceId = Lens.lens (instanceId :: ElasticGpus -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ElasticGpus)
{-# DEPRECATED egInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuType :: Lens.Lens' ElasticGpus (Lude.Maybe Lude.Text)
egElasticGpuType = Lens.lens (elasticGpuType :: ElasticGpus -> Lude.Maybe Lude.Text) (\s a -> s {elasticGpuType = a} :: ElasticGpus)
{-# DEPRECATED egElasticGpuType "Use generic-lens or generic-optics with 'elasticGpuType' instead." #-}

-- | The ID of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuId :: Lens.Lens' ElasticGpus (Lude.Maybe Lude.Text)
egElasticGpuId = Lens.lens (elasticGpuId :: ElasticGpus -> Lude.Maybe Lude.Text) (\s a -> s {elasticGpuId = a} :: ElasticGpus)
{-# DEPRECATED egElasticGpuId "Use generic-lens or generic-optics with 'elasticGpuId' instead." #-}

-- | The state of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuState :: Lens.Lens' ElasticGpus (Lude.Maybe ElasticGpuState)
egElasticGpuState = Lens.lens (elasticGpuState :: ElasticGpus -> Lude.Maybe ElasticGpuState) (\s a -> s {elasticGpuState = a} :: ElasticGpus)
{-# DEPRECATED egElasticGpuState "Use generic-lens or generic-optics with 'elasticGpuState' instead." #-}

-- | The status of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuHealth :: Lens.Lens' ElasticGpus (Lude.Maybe ElasticGpuHealth)
egElasticGpuHealth = Lens.lens (elasticGpuHealth :: ElasticGpus -> Lude.Maybe ElasticGpuHealth) (\s a -> s {elasticGpuHealth = a} :: ElasticGpus)
{-# DEPRECATED egElasticGpuHealth "Use generic-lens or generic-optics with 'elasticGpuHealth' instead." #-}

-- | The Availability Zone in the which the Elastic Graphics accelerator resides.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egAvailabilityZone :: Lens.Lens' ElasticGpus (Lude.Maybe Lude.Text)
egAvailabilityZone = Lens.lens (availabilityZone :: ElasticGpus -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ElasticGpus)
{-# DEPRECATED egAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The tags assigned to the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egTags :: Lens.Lens' ElasticGpus (Lude.Maybe [Tag])
egTags = Lens.lens (tags :: ElasticGpus -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ElasticGpus)
{-# DEPRECATED egTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ElasticGpus where
  parseXML x =
    ElasticGpus'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "elasticGpuType")
      Lude.<*> (x Lude..@? "elasticGpuId")
      Lude.<*> (x Lude..@? "elasticGpuState")
      Lude.<*> (x Lude..@? "elasticGpuHealth")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
