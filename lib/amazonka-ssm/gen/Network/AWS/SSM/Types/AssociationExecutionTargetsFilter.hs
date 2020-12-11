-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
  ( AssociationExecutionTargetsFilter (..),

    -- * Smart constructor
    mkAssociationExecutionTargetsFilter,

    -- * Lenses
    aetfKey,
    aetfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey

-- | Filters for the association execution.
--
-- /See:/ 'mkAssociationExecutionTargetsFilter' smart constructor.
data AssociationExecutionTargetsFilter = AssociationExecutionTargetsFilter'
  { key ::
      AssociationExecutionTargetsFilterKey,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationExecutionTargetsFilter' with the minimum fields required to make a request.
--
-- * 'key' - The key value used in the request.
-- * 'value' - The value specified for the key.
mkAssociationExecutionTargetsFilter ::
  -- | 'key'
  AssociationExecutionTargetsFilterKey ->
  -- | 'value'
  Lude.Text ->
  AssociationExecutionTargetsFilter
mkAssociationExecutionTargetsFilter pKey_ pValue_ =
  AssociationExecutionTargetsFilter' {key = pKey_, value = pValue_}

-- | The key value used in the request.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetfKey :: Lens.Lens' AssociationExecutionTargetsFilter AssociationExecutionTargetsFilterKey
aetfKey = Lens.lens (key :: AssociationExecutionTargetsFilter -> AssociationExecutionTargetsFilterKey) (\s a -> s {key = a} :: AssociationExecutionTargetsFilter)
{-# DEPRECATED aetfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value specified for the key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetfValue :: Lens.Lens' AssociationExecutionTargetsFilter Lude.Text
aetfValue = Lens.lens (value :: AssociationExecutionTargetsFilter -> Lude.Text) (\s a -> s {value = a} :: AssociationExecutionTargetsFilter)
{-# DEPRECATED aetfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON AssociationExecutionTargetsFilter where
  toJSON AssociationExecutionTargetsFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Key" Lude..= key), Lude.Just ("Value" Lude..= value)]
      )
