{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDeleteObject
  ( BatchDeleteObject (..),

    -- * Smart constructor
    mkBatchDeleteObject,

    -- * Lenses
    bdoObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'DeleteObject' operation.
--
-- /See:/ 'mkBatchDeleteObject' smart constructor.
newtype BatchDeleteObject = BatchDeleteObject'
  { objectReference ::
      ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteObject' with the minimum fields required to make a request.
--
-- * 'objectReference' - The reference that identifies the object.
mkBatchDeleteObject ::
  -- | 'objectReference'
  ObjectReference ->
  BatchDeleteObject
mkBatchDeleteObject pObjectReference_ =
  BatchDeleteObject' {objectReference = pObjectReference_}

-- | The reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoObjectReference :: Lens.Lens' BatchDeleteObject ObjectReference
bdoObjectReference = Lens.lens (objectReference :: BatchDeleteObject -> ObjectReference) (\s a -> s {objectReference = a} :: BatchDeleteObject)
{-# DEPRECATED bdoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchDeleteObject where
  toJSON BatchDeleteObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ObjectReference" Lude..= objectReference)]
      )
