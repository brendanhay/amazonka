{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAccelerator
  ( ElasticInferenceAccelerator (..),

    -- * Smart constructor
    mkElasticInferenceAccelerator,

    -- * Lenses
    eiaCount,
    eiaType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'mkElasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { count ::
      Lude.Maybe Lude.Natural,
    type' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticInferenceAccelerator' with the minimum fields required to make a request.
--
-- * 'count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
-- * 'type'' - The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ .
mkElasticInferenceAccelerator ::
  -- | 'type''
  Lude.Text ->
  ElasticInferenceAccelerator
mkElasticInferenceAccelerator pType_ =
  ElasticInferenceAccelerator'
    { count = Lude.Nothing,
      type' = pType_
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaCount :: Lens.Lens' ElasticInferenceAccelerator (Lude.Maybe Lude.Natural)
eiaCount = Lens.lens (count :: ElasticInferenceAccelerator -> Lude.Maybe Lude.Natural) (\s a -> s {count = a} :: ElasticInferenceAccelerator)
{-# DEPRECATED eiaCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaType :: Lens.Lens' ElasticInferenceAccelerator Lude.Text
eiaType = Lens.lens (type' :: ElasticInferenceAccelerator -> Lude.Text) (\s a -> s {type' = a} :: ElasticInferenceAccelerator)
{-# DEPRECATED eiaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToQuery ElasticInferenceAccelerator where
  toQuery ElasticInferenceAccelerator' {..} =
    Lude.mconcat ["Count" Lude.=: count, "Type" Lude.=: type']
