{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
  ( LaunchTemplateElasticInferenceAcceleratorResponse (..),

    -- * Smart constructor
    mkLaunchTemplateElasticInferenceAcceleratorResponse,

    -- * Lenses
    lCount,
    lType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'mkLaunchTemplateElasticInferenceAcceleratorResponse' smart constructor.
data LaunchTemplateElasticInferenceAcceleratorResponse = LaunchTemplateElasticInferenceAcceleratorResponse'
  { -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Lude.Maybe Lude.Int,
    -- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateElasticInferenceAcceleratorResponse' with the minimum fields required to make a request.
--
-- * 'count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
-- * 'type'' - The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
mkLaunchTemplateElasticInferenceAcceleratorResponse ::
  LaunchTemplateElasticInferenceAcceleratorResponse
mkLaunchTemplateElasticInferenceAcceleratorResponse =
  LaunchTemplateElasticInferenceAcceleratorResponse'
    { count =
        Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCount :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Lude.Maybe Lude.Int)
lCount = Lens.lens (count :: LaunchTemplateElasticInferenceAcceleratorResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: LaunchTemplateElasticInferenceAcceleratorResponse)
{-# DEPRECATED lCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Lude.Maybe Lude.Text)
lType = Lens.lens (type' :: LaunchTemplateElasticInferenceAcceleratorResponse -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: LaunchTemplateElasticInferenceAcceleratorResponse)
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance
  Lude.FromXML
    LaunchTemplateElasticInferenceAcceleratorResponse
  where
  parseXML x =
    LaunchTemplateElasticInferenceAcceleratorResponse'
      Lude.<$> (x Lude..@? "count") Lude.<*> (x Lude..@? "type")
