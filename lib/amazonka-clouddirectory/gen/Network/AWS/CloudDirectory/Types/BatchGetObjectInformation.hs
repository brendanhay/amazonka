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

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Retrieves metadata about an object inside a 'BatchRead' operation. For more information, see 'GetObjectInformation' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchGetObjectInformation' smart constructor.
newtype BatchGetObjectInformation = BatchGetObjectInformation'
  { -- | A reference to the object.
    objectReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetObjectInformation' value with any optional fields omitted.
mkBatchGetObjectInformation ::
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchGetObjectInformation
mkBatchGetObjectInformation objectReference =
  BatchGetObjectInformation' {objectReference}

-- | A reference to the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoiObjectReference :: Lens.Lens' BatchGetObjectInformation Types.ObjectReference
bgoiObjectReference = Lens.field @"objectReference"
{-# DEPRECATED bgoiObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Core.FromJSON BatchGetObjectInformation where
  toJSON BatchGetObjectInformation {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ObjectReference" Core..= objectReference)]
      )
