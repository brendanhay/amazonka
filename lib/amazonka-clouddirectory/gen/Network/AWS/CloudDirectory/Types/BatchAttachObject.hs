{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachObject
  ( BatchAttachObject (..),

    -- * Smart constructor
    mkBatchAttachObject,

    -- * Lenses
    baoParentReference,
    baoChildReference,
    baoLinkName,
  )
where

import qualified Network.AWS.CloudDirectory.Types.LinkName as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of an 'AttachObject' operation.
--
-- /See:/ 'mkBatchAttachObject' smart constructor.
data BatchAttachObject = BatchAttachObject'
  { -- | The parent object reference.
    parentReference :: Types.ObjectReference,
    -- | The child object reference that is to be attached to the object.
    childReference :: Types.ObjectReference,
    -- | The name of the link.
    linkName :: Types.LinkName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAttachObject' value with any optional fields omitted.
mkBatchAttachObject ::
  -- | 'parentReference'
  Types.ObjectReference ->
  -- | 'childReference'
  Types.ObjectReference ->
  -- | 'linkName'
  Types.LinkName ->
  BatchAttachObject
mkBatchAttachObject parentReference childReference linkName =
  BatchAttachObject' {parentReference, childReference, linkName}

-- | The parent object reference.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoParentReference :: Lens.Lens' BatchAttachObject Types.ObjectReference
baoParentReference = Lens.field @"parentReference"
{-# DEPRECATED baoParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The child object reference that is to be attached to the object.
--
-- /Note:/ Consider using 'childReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoChildReference :: Lens.Lens' BatchAttachObject Types.ObjectReference
baoChildReference = Lens.field @"childReference"
{-# DEPRECATED baoChildReference "Use generic-lens or generic-optics with 'childReference' instead." #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoLinkName :: Lens.Lens' BatchAttachObject Types.LinkName
baoLinkName = Lens.field @"linkName"
{-# DEPRECATED baoLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Core.FromJSON BatchAttachObject where
  toJSON BatchAttachObject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParentReference" Core..= parentReference),
            Core.Just ("ChildReference" Core..= childReference),
            Core.Just ("LinkName" Core..= linkName)
          ]
      )
