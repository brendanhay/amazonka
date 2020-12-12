{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
  ( BatchAttachObjectResponse (..),

    -- * Smart constructor
    mkBatchAttachObjectResponse,

    -- * Lenses
    baoAttachedObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output batch 'AttachObject' response operation.
--
-- /See:/ 'mkBatchAttachObjectResponse' smart constructor.
newtype BatchAttachObjectResponse = BatchAttachObjectResponse'
  { attachedObjectIdentifier ::
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

-- | Creates a value of 'BatchAttachObjectResponse' with the minimum fields required to make a request.
--
-- * 'attachedObjectIdentifier' - The @ObjectIdentifier@ of the object that has been attached.
mkBatchAttachObjectResponse ::
  BatchAttachObjectResponse
mkBatchAttachObjectResponse =
  BatchAttachObjectResponse'
    { attachedObjectIdentifier =
        Lude.Nothing
    }

-- | The @ObjectIdentifier@ of the object that has been attached.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baoAttachedObjectIdentifier :: Lens.Lens' BatchAttachObjectResponse (Lude.Maybe Lude.Text)
baoAttachedObjectIdentifier = Lens.lens (attachedObjectIdentifier :: BatchAttachObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {attachedObjectIdentifier = a} :: BatchAttachObjectResponse)
{-# DEPRECATED baoAttachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead." #-}

instance Lude.FromJSON BatchAttachObjectResponse where
  parseJSON =
    Lude.withObject
      "BatchAttachObjectResponse"
      ( \x ->
          BatchAttachObjectResponse'
            Lude.<$> (x Lude..:? "attachedObjectIdentifier")
      )
