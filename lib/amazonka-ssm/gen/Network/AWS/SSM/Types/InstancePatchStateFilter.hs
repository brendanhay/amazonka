{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchStateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchStateFilter
  ( InstancePatchStateFilter (..),

    -- * Smart constructor
    mkInstancePatchStateFilter,

    -- * Lenses
    ipsfKey,
    ipsfValues,
    ipsfType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InstancePatchStateOperatorType

-- | Defines a filter used in 'DescribeInstancePatchStatesForPatchGroup' used to scope down the information returned by the API.
--
-- /See:/ 'mkInstancePatchStateFilter' smart constructor.
data InstancePatchStateFilter = InstancePatchStateFilter'
  { key ::
      Lude.Text,
    values :: Lude.NonEmpty Lude.Text,
    type' :: InstancePatchStateOperatorType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstancePatchStateFilter' with the minimum fields required to make a request.
--
-- * 'key' - The key for the filter. Supported values are FailedCount, InstalledCount, InstalledOtherCount, MissingCount and NotApplicableCount.
-- * 'type'' - The type of comparison that should be performed for the value: Equal, NotEqual, LessThan or GreaterThan.
-- * 'values' - The value for the filter, must be an integer greater than or equal to 0.
mkInstancePatchStateFilter ::
  -- | 'key'
  Lude.Text ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  -- | 'type''
  InstancePatchStateOperatorType ->
  InstancePatchStateFilter
mkInstancePatchStateFilter pKey_ pValues_ pType_ =
  InstancePatchStateFilter'
    { key = pKey_,
      values = pValues_,
      type' = pType_
    }

-- | The key for the filter. Supported values are FailedCount, InstalledCount, InstalledOtherCount, MissingCount and NotApplicableCount.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsfKey :: Lens.Lens' InstancePatchStateFilter Lude.Text
ipsfKey = Lens.lens (key :: InstancePatchStateFilter -> Lude.Text) (\s a -> s {key = a} :: InstancePatchStateFilter)
{-# DEPRECATED ipsfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value for the filter, must be an integer greater than or equal to 0.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsfValues :: Lens.Lens' InstancePatchStateFilter (Lude.NonEmpty Lude.Text)
ipsfValues = Lens.lens (values :: InstancePatchStateFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: InstancePatchStateFilter)
{-# DEPRECATED ipsfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The type of comparison that should be performed for the value: Equal, NotEqual, LessThan or GreaterThan.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsfType :: Lens.Lens' InstancePatchStateFilter InstancePatchStateOperatorType
ipsfType = Lens.lens (type' :: InstancePatchStateFilter -> InstancePatchStateOperatorType) (\s a -> s {type' = a} :: InstancePatchStateFilter)
{-# DEPRECATED ipsfType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON InstancePatchStateFilter where
  toJSON InstancePatchStateFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values),
            Lude.Just ("Type" Lude..= type')
          ]
      )
