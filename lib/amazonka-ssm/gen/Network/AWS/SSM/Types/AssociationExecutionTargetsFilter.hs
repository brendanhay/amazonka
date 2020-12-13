{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    aetfValue,
    aetfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey

-- | Filters for the association execution.
--
-- /See:/ 'mkAssociationExecutionTargetsFilter' smart constructor.
data AssociationExecutionTargetsFilter = AssociationExecutionTargetsFilter'
  { -- | The value specified for the key.
    value :: Lude.Text,
    -- | The key value used in the request.
    key :: AssociationExecutionTargetsFilterKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationExecutionTargetsFilter' with the minimum fields required to make a request.
--
-- * 'value' - The value specified for the key.
-- * 'key' - The key value used in the request.
mkAssociationExecutionTargetsFilter ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  AssociationExecutionTargetsFilterKey ->
  AssociationExecutionTargetsFilter
mkAssociationExecutionTargetsFilter pValue_ pKey_ =
  AssociationExecutionTargetsFilter' {value = pValue_, key = pKey_}

-- | The value specified for the key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetfValue :: Lens.Lens' AssociationExecutionTargetsFilter Lude.Text
aetfValue = Lens.lens (value :: AssociationExecutionTargetsFilter -> Lude.Text) (\s a -> s {value = a} :: AssociationExecutionTargetsFilter)
{-# DEPRECATED aetfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key value used in the request.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetfKey :: Lens.Lens' AssociationExecutionTargetsFilter AssociationExecutionTargetsFilterKey
aetfKey = Lens.lens (key :: AssociationExecutionTargetsFilter -> AssociationExecutionTargetsFilterKey) (\s a -> s {key = a} :: AssociationExecutionTargetsFilter)
{-# DEPRECATED aetfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON AssociationExecutionTargetsFilter where
  toJSON AssociationExecutionTargetsFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Value" Lude..= value), Lude.Just ("Key" Lude..= key)]
      )
