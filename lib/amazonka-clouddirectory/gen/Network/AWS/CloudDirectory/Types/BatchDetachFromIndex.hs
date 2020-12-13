{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
  ( BatchDetachFromIndex (..),

    -- * Smart constructor
    mkBatchDetachFromIndex,

    -- * Lenses
    bdfiTargetReference,
    bdfiIndexReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detaches the specified object from the specified index inside a 'BatchRead' operation. For more information, see 'DetachFromIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchDetachFromIndex' smart constructor.
data BatchDetachFromIndex = BatchDetachFromIndex'
  { -- | A reference to the object being detached from the index.
    targetReference :: ObjectReference,
    -- | A reference to the index object.
    indexReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachFromIndex' with the minimum fields required to make a request.
--
-- * 'targetReference' - A reference to the object being detached from the index.
-- * 'indexReference' - A reference to the index object.
mkBatchDetachFromIndex ::
  -- | 'targetReference'
  ObjectReference ->
  -- | 'indexReference'
  ObjectReference ->
  BatchDetachFromIndex
mkBatchDetachFromIndex pTargetReference_ pIndexReference_ =
  BatchDetachFromIndex'
    { targetReference = pTargetReference_,
      indexReference = pIndexReference_
    }

-- | A reference to the object being detached from the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdfiTargetReference :: Lens.Lens' BatchDetachFromIndex ObjectReference
bdfiTargetReference = Lens.lens (targetReference :: BatchDetachFromIndex -> ObjectReference) (\s a -> s {targetReference = a} :: BatchDetachFromIndex)
{-# DEPRECATED bdfiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

-- | A reference to the index object.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdfiIndexReference :: Lens.Lens' BatchDetachFromIndex ObjectReference
bdfiIndexReference = Lens.lens (indexReference :: BatchDetachFromIndex -> ObjectReference) (\s a -> s {indexReference = a} :: BatchDetachFromIndex)
{-# DEPRECATED bdfiIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

instance Lude.ToJSON BatchDetachFromIndex where
  toJSON BatchDetachFromIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetReference" Lude..= targetReference),
            Lude.Just ("IndexReference" Lude..= indexReference)
          ]
      )
