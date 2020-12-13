{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
  ( BatchDetachFromIndexResponse (..),

    -- * Smart constructor
    mkBatchDetachFromIndexResponse,

    -- * Lenses
    bdfiDetachedObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'DetachFromIndex' response operation.
--
-- /See:/ 'mkBatchDetachFromIndexResponse' smart constructor.
newtype BatchDetachFromIndexResponse = BatchDetachFromIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was detached from the index.
    detachedObjectIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachFromIndexResponse' with the minimum fields required to make a request.
--
-- * 'detachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was detached from the index.
mkBatchDetachFromIndexResponse ::
  BatchDetachFromIndexResponse
mkBatchDetachFromIndexResponse =
  BatchDetachFromIndexResponse'
    { detachedObjectIdentifier =
        Lude.Nothing
    }

-- | The @ObjectIdentifier@ of the object that was detached from the index.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdfiDetachedObjectIdentifier :: Lens.Lens' BatchDetachFromIndexResponse (Lude.Maybe Lude.Text)
bdfiDetachedObjectIdentifier = Lens.lens (detachedObjectIdentifier :: BatchDetachFromIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {detachedObjectIdentifier = a} :: BatchDetachFromIndexResponse)
{-# DEPRECATED bdfiDetachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead." #-}

instance Lude.FromJSON BatchDetachFromIndexResponse where
  parseJSON =
    Lude.withObject
      "BatchDetachFromIndexResponse"
      ( \x ->
          BatchDetachFromIndexResponse'
            Lude.<$> (x Lude..:? "DetachedObjectIdentifier")
      )
