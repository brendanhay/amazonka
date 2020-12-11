-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionFilter
  ( AssociationExecutionFilter (..),

    -- * Smart constructor
    mkAssociationExecutionFilter,

    -- * Lenses
    aefKey,
    aefValue,
    aefType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationExecutionFilterKey
import Network.AWS.SSM.Types.AssociationFilterOperatorType

-- | Filters used in the request.
--
-- /See:/ 'mkAssociationExecutionFilter' smart constructor.
data AssociationExecutionFilter = AssociationExecutionFilter'
  { key ::
      AssociationExecutionFilterKey,
    value :: Lude.Text,
    type' ::
      AssociationFilterOperatorType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationExecutionFilter' with the minimum fields required to make a request.
--
-- * 'key' - The key value used in the request.
-- * 'type'' - The filter type specified in the request.
-- * 'value' - The value specified for the key.
mkAssociationExecutionFilter ::
  -- | 'key'
  AssociationExecutionFilterKey ->
  -- | 'value'
  Lude.Text ->
  -- | 'type''
  AssociationFilterOperatorType ->
  AssociationExecutionFilter
mkAssociationExecutionFilter pKey_ pValue_ pType_ =
  AssociationExecutionFilter'
    { key = pKey_,
      value = pValue_,
      type' = pType_
    }

-- | The key value used in the request.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefKey :: Lens.Lens' AssociationExecutionFilter AssociationExecutionFilterKey
aefKey = Lens.lens (key :: AssociationExecutionFilter -> AssociationExecutionFilterKey) (\s a -> s {key = a} :: AssociationExecutionFilter)
{-# DEPRECATED aefKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value specified for the key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefValue :: Lens.Lens' AssociationExecutionFilter Lude.Text
aefValue = Lens.lens (value :: AssociationExecutionFilter -> Lude.Text) (\s a -> s {value = a} :: AssociationExecutionFilter)
{-# DEPRECATED aefValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The filter type specified in the request.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefType :: Lens.Lens' AssociationExecutionFilter AssociationFilterOperatorType
aefType = Lens.lens (type' :: AssociationExecutionFilter -> AssociationFilterOperatorType) (\s a -> s {type' = a} :: AssociationExecutionFilter)
{-# DEPRECATED aefType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON AssociationExecutionFilter where
  toJSON AssociationExecutionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Value" Lude..= value),
            Lude.Just ("Type" Lude..= type')
          ]
      )
