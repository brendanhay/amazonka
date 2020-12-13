{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
  ( BatchAttachToIndexResponse (..),

    -- * Smart constructor
    mkBatchAttachToIndexResponse,

    -- * Lenses
    batiAttachedObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'AttachToIndex' response operation.
--
-- /See:/ 'mkBatchAttachToIndexResponse' smart constructor.
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAttachToIndexResponse' with the minimum fields required to make a request.
--
-- * 'attachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
mkBatchAttachToIndexResponse ::
  BatchAttachToIndexResponse
mkBatchAttachToIndexResponse =
  BatchAttachToIndexResponse'
    { attachedObjectIdentifier =
        Lude.Nothing
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batiAttachedObjectIdentifier :: Lens.Lens' BatchAttachToIndexResponse (Lude.Maybe Lude.Text)
batiAttachedObjectIdentifier = Lens.lens (attachedObjectIdentifier :: BatchAttachToIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {attachedObjectIdentifier = a} :: BatchAttachToIndexResponse)
{-# DEPRECATED batiAttachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead." #-}

instance Lude.FromJSON BatchAttachToIndexResponse where
  parseJSON =
    Lude.withObject
      "BatchAttachToIndexResponse"
      ( \x ->
          BatchAttachToIndexResponse'
            Lude.<$> (x Lude..:? "AttachedObjectIdentifier")
      )
