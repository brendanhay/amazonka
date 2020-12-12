{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
  ( BatchGetObjectInformation (..),

    -- * Smart constructor
    mkBatchGetObjectInformation,

    -- * Lenses
    bgoiObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Retrieves metadata about an object inside a 'BatchRead' operation. For more information, see 'GetObjectInformation' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchGetObjectInformation' smart constructor.
newtype BatchGetObjectInformation = BatchGetObjectInformation'
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

-- | Creates a value of 'BatchGetObjectInformation' with the minimum fields required to make a request.
--
-- * 'objectReference' - A reference to the object.
mkBatchGetObjectInformation ::
  -- | 'objectReference'
  ObjectReference ->
  BatchGetObjectInformation
mkBatchGetObjectInformation pObjectReference_ =
  BatchGetObjectInformation' {objectReference = pObjectReference_}

-- | A reference to the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoiObjectReference :: Lens.Lens' BatchGetObjectInformation ObjectReference
bgoiObjectReference = Lens.lens (objectReference :: BatchGetObjectInformation -> ObjectReference) (\s a -> s {objectReference = a} :: BatchGetObjectInformation)
{-# DEPRECATED bgoiObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchGetObjectInformation where
  toJSON BatchGetObjectInformation' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ObjectReference" Lude..= objectReference)]
      )
