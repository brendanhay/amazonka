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

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of an 'AttachObject' operation.
--
-- /See:/ 'mkBatchAttachObject' smart constructor.
data BatchAttachObject = BatchAttachObject'
  { parentReference ::
      ObjectReference,
    childReference :: ObjectReference,
    linkName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAttachObject' with the minimum fields required to make a request.
--
-- * 'childReference' - The child object reference that is to be attached to the object.
-- * 'linkName' - The name of the link.
-- * 'parentReference' - The parent object reference.
mkBatchAttachObject ::
  -- | 'parentReference'
  ObjectReference ->
  -- | 'childReference'
  ObjectReference ->
  -- | 'linkName'
  Lude.Text ->
  BatchAttachObject
mkBatchAttachObject pParentReference_ pChildReference_ pLinkName_ =
  BatchAttachObject'
    { parentReference = pParentReference_,
      childReference = pChildReference_,
      linkName = pLinkName_
    }

-- | The parent object reference.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoParentReference :: Lens.Lens' BatchAttachObject ObjectReference
baoParentReference = Lens.lens (parentReference :: BatchAttachObject -> ObjectReference) (\s a -> s {parentReference = a} :: BatchAttachObject)
{-# DEPRECATED baoParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The child object reference that is to be attached to the object.
--
-- /Note:/ Consider using 'childReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoChildReference :: Lens.Lens' BatchAttachObject ObjectReference
baoChildReference = Lens.lens (childReference :: BatchAttachObject -> ObjectReference) (\s a -> s {childReference = a} :: BatchAttachObject)
{-# DEPRECATED baoChildReference "Use generic-lens or generic-optics with 'childReference' instead." #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoLinkName :: Lens.Lens' BatchAttachObject Lude.Text
baoLinkName = Lens.lens (linkName :: BatchAttachObject -> Lude.Text) (\s a -> s {linkName = a} :: BatchAttachObject)
{-# DEPRECATED baoLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Lude.ToJSON BatchAttachObject where
  toJSON BatchAttachObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParentReference" Lude..= parentReference),
            Lude.Just ("ChildReference" Lude..= childReference),
            Lude.Just ("LinkName" Lude..= linkName)
          ]
      )
