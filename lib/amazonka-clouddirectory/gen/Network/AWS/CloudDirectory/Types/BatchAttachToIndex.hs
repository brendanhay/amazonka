{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachToIndex
  ( BatchAttachToIndex (..),

    -- * Smart constructor
    mkBatchAttachToIndex,

    -- * Lenses
    batiIndexReference,
    batiTargetReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attaches the specified object to the specified index inside a 'BatchRead' operation. For more information, see 'AttachToIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchAttachToIndex' smart constructor.
data BatchAttachToIndex = BatchAttachToIndex'
  { indexReference ::
      ObjectReference,
    targetReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAttachToIndex' with the minimum fields required to make a request.
--
-- * 'indexReference' - A reference to the index that you are attaching the object to.
-- * 'targetReference' - A reference to the object that you are attaching to the index.
mkBatchAttachToIndex ::
  -- | 'indexReference'
  ObjectReference ->
  -- | 'targetReference'
  ObjectReference ->
  BatchAttachToIndex
mkBatchAttachToIndex pIndexReference_ pTargetReference_ =
  BatchAttachToIndex'
    { indexReference = pIndexReference_,
      targetReference = pTargetReference_
    }

-- | A reference to the index that you are attaching the object to.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batiIndexReference :: Lens.Lens' BatchAttachToIndex ObjectReference
batiIndexReference = Lens.lens (indexReference :: BatchAttachToIndex -> ObjectReference) (\s a -> s {indexReference = a} :: BatchAttachToIndex)
{-# DEPRECATED batiIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | A reference to the object that you are attaching to the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batiTargetReference :: Lens.Lens' BatchAttachToIndex ObjectReference
batiTargetReference = Lens.lens (targetReference :: BatchAttachToIndex -> ObjectReference) (\s a -> s {targetReference = a} :: BatchAttachToIndex)
{-# DEPRECATED batiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

instance Lude.ToJSON BatchAttachToIndex where
  toJSON BatchAttachToIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IndexReference" Lude..= indexReference),
            Lude.Just ("TargetReference" Lude..= targetReference)
          ]
      )
