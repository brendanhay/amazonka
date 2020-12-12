{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
  ( BatchDetachObjectResponse (..),

    -- * Smart constructor
    mkBatchDetachObjectResponse,

    -- * Lenses
    bdoDetachedObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'DetachObject' response operation.
--
-- /See:/ 'mkBatchDetachObjectResponse' smart constructor.
newtype BatchDetachObjectResponse = BatchDetachObjectResponse'
  { detachedObjectIdentifier ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachObjectResponse' with the minimum fields required to make a request.
--
-- * 'detachedObjectIdentifier' - The @ObjectIdentifier@ of the detached object.
mkBatchDetachObjectResponse ::
  BatchDetachObjectResponse
mkBatchDetachObjectResponse =
  BatchDetachObjectResponse'
    { detachedObjectIdentifier =
        Lude.Nothing
    }

-- | The @ObjectIdentifier@ of the detached object.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoDetachedObjectIdentifier :: Lens.Lens' BatchDetachObjectResponse (Lude.Maybe Lude.Text)
bdoDetachedObjectIdentifier = Lens.lens (detachedObjectIdentifier :: BatchDetachObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {detachedObjectIdentifier = a} :: BatchDetachObjectResponse)
{-# DEPRECATED bdoDetachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead." #-}

instance Lude.FromJSON BatchDetachObjectResponse where
  parseJSON =
    Lude.withObject
      "BatchDetachObjectResponse"
      ( \x ->
          BatchDetachObjectResponse'
            Lude.<$> (x Lude..:? "detachedObjectIdentifier")
      )
