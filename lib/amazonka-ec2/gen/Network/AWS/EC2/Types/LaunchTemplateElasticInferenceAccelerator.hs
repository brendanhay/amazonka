-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
  ( LaunchTemplateElasticInferenceAccelerator (..),

    -- * Smart constructor
    mkLaunchTemplateElasticInferenceAccelerator,

    -- * Lenses
    lteiaCount,
    lteiaType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'mkLaunchTemplateElasticInferenceAccelerator' smart constructor.
data LaunchTemplateElasticInferenceAccelerator = LaunchTemplateElasticInferenceAccelerator'
  { count ::
      Lude.Maybe
        Lude.Natural,
    type' ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateElasticInferenceAccelerator' with the minimum fields required to make a request.
--
-- * 'count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
-- * 'type'' - The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
mkLaunchTemplateElasticInferenceAccelerator ::
  -- | 'type''
  Lude.Text ->
  LaunchTemplateElasticInferenceAccelerator
mkLaunchTemplateElasticInferenceAccelerator pType_ =
  LaunchTemplateElasticInferenceAccelerator'
    { count = Lude.Nothing,
      type' = pType_
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiaCount :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator (Lude.Maybe Lude.Natural)
lteiaCount = Lens.lens (count :: LaunchTemplateElasticInferenceAccelerator -> Lude.Maybe Lude.Natural) (\s a -> s {count = a} :: LaunchTemplateElasticInferenceAccelerator)
{-# DEPRECATED lteiaCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiaType :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator Lude.Text
lteiaType = Lens.lens (type' :: LaunchTemplateElasticInferenceAccelerator -> Lude.Text) (\s a -> s {type' = a} :: LaunchTemplateElasticInferenceAccelerator)
{-# DEPRECATED lteiaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToQuery LaunchTemplateElasticInferenceAccelerator where
  toQuery LaunchTemplateElasticInferenceAccelerator' {..} =
    Lude.mconcat ["Count" Lude.=: count, "Type" Lude.=: type']
