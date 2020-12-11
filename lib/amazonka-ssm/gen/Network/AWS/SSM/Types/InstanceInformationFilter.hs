-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilter
  ( InstanceInformationFilter (..),

    -- * Smart constructor
    mkInstanceInformationFilter,

    -- * Lenses
    iifKey,
    iifValueSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InstanceInformationFilterKey

-- | Describes a filter for a specific list of instances. You can filter instances information by using tags. You specify tags by using a key-value mapping.
--
-- Use this action instead of the 'DescribeInstanceInformationRequest$InstanceInformationFilterList' method. The @InstanceInformationFilterList@ method is a legacy method and does not support tags.
--
-- /See:/ 'mkInstanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { key ::
      InstanceInformationFilterKey,
    valueSet :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceInformationFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'valueSet' - The filter values.
mkInstanceInformationFilter ::
  -- | 'key'
  InstanceInformationFilterKey ->
  -- | 'valueSet'
  Lude.NonEmpty Lude.Text ->
  InstanceInformationFilter
mkInstanceInformationFilter pKey_ pValueSet_ =
  InstanceInformationFilter' {key = pKey_, valueSet = pValueSet_}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifKey :: Lens.Lens' InstanceInformationFilter InstanceInformationFilterKey
iifKey = Lens.lens (key :: InstanceInformationFilter -> InstanceInformationFilterKey) (\s a -> s {key = a} :: InstanceInformationFilter)
{-# DEPRECATED iifKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'valueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifValueSet :: Lens.Lens' InstanceInformationFilter (Lude.NonEmpty Lude.Text)
iifValueSet = Lens.lens (valueSet :: InstanceInformationFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {valueSet = a} :: InstanceInformationFilter)
{-# DEPRECATED iifValueSet "Use generic-lens or generic-optics with 'valueSet' instead." #-}

instance Lude.ToJSON InstanceInformationFilter where
  toJSON InstanceInformationFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("key" Lude..= key),
            Lude.Just ("valueSet" Lude..= valueSet)
          ]
      )
