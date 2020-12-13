{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachObject
  ( BatchDetachObject (..),

    -- * Smart constructor
    mkBatchDetachObject,

    -- * Lenses
    bdoParentReference,
    bdoLinkName,
    bdoBatchReferenceName,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'DetachObject' operation.
--
-- /See:/ 'mkBatchDetachObject' smart constructor.
data BatchDetachObject = BatchDetachObject'
  { -- | Parent reference from which the object with the specified link name is detached.
    parentReference :: ObjectReference,
    -- | The name of the link.
    linkName :: Lude.Text,
    -- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
    batchReferenceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachObject' with the minimum fields required to make a request.
--
-- * 'parentReference' - Parent reference from which the object with the specified link name is detached.
-- * 'linkName' - The name of the link.
-- * 'batchReferenceName' - The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
mkBatchDetachObject ::
  -- | 'parentReference'
  ObjectReference ->
  -- | 'linkName'
  Lude.Text ->
  BatchDetachObject
mkBatchDetachObject pParentReference_ pLinkName_ =
  BatchDetachObject'
    { parentReference = pParentReference_,
      linkName = pLinkName_,
      batchReferenceName = Lude.Nothing
    }

-- | Parent reference from which the object with the specified link name is detached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoParentReference :: Lens.Lens' BatchDetachObject ObjectReference
bdoParentReference = Lens.lens (parentReference :: BatchDetachObject -> ObjectReference) (\s a -> s {parentReference = a} :: BatchDetachObject)
{-# DEPRECATED bdoParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoLinkName :: Lens.Lens' BatchDetachObject Lude.Text
bdoLinkName = Lens.lens (linkName :: BatchDetachObject -> Lude.Text) (\s a -> s {linkName = a} :: BatchDetachObject)
{-# DEPRECATED bdoLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- /Note:/ Consider using 'batchReferenceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoBatchReferenceName :: Lens.Lens' BatchDetachObject (Lude.Maybe Lude.Text)
bdoBatchReferenceName = Lens.lens (batchReferenceName :: BatchDetachObject -> Lude.Maybe Lude.Text) (\s a -> s {batchReferenceName = a} :: BatchDetachObject)
{-# DEPRECATED bdoBatchReferenceName "Use generic-lens or generic-optics with 'batchReferenceName' instead." #-}

instance Lude.ToJSON BatchDetachObject where
  toJSON BatchDetachObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParentReference" Lude..= parentReference),
            Lude.Just ("LinkName" Lude..= linkName),
            ("BatchReferenceName" Lude..=) Lude.<$> batchReferenceName
          ]
      )
