{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AttachmentStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AttachmentStateChange
  ( AttachmentStateChange (..),

    -- * Smart constructor
    mkAttachmentStateChange,

    -- * Lenses
    ascStatus,
    ascAttachmentARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a change in state for a task attachment.
--
-- /See:/ 'mkAttachmentStateChange' smart constructor.
data AttachmentStateChange = AttachmentStateChange'
  { -- | The status of the attachment.
    status :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the attachment.
    attachmentARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachmentStateChange' with the minimum fields required to make a request.
--
-- * 'status' - The status of the attachment.
-- * 'attachmentARN' - The Amazon Resource Name (ARN) of the attachment.
mkAttachmentStateChange ::
  -- | 'status'
  Lude.Text ->
  -- | 'attachmentARN'
  Lude.Text ->
  AttachmentStateChange
mkAttachmentStateChange pStatus_ pAttachmentARN_ =
  AttachmentStateChange'
    { status = pStatus_,
      attachmentARN = pAttachmentARN_
    }

-- | The status of the attachment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascStatus :: Lens.Lens' AttachmentStateChange Lude.Text
ascStatus = Lens.lens (status :: AttachmentStateChange -> Lude.Text) (\s a -> s {status = a} :: AttachmentStateChange)
{-# DEPRECATED ascStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the attachment.
--
-- /Note:/ Consider using 'attachmentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascAttachmentARN :: Lens.Lens' AttachmentStateChange Lude.Text
ascAttachmentARN = Lens.lens (attachmentARN :: AttachmentStateChange -> Lude.Text) (\s a -> s {attachmentARN = a} :: AttachmentStateChange)
{-# DEPRECATED ascAttachmentARN "Use generic-lens or generic-optics with 'attachmentARN' instead." #-}

instance Lude.ToJSON AttachmentStateChange where
  toJSON AttachmentStateChange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("status" Lude..= status),
            Lude.Just ("attachmentArn" Lude..= attachmentARN)
          ]
      )
