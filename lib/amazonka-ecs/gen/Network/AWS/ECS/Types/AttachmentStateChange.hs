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
    ascAttachmentArn,
    ascStatus,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a change in state for a task attachment.
--
-- /See:/ 'mkAttachmentStateChange' smart constructor.
data AttachmentStateChange = AttachmentStateChange'
  { -- | The Amazon Resource Name (ARN) of the attachment.
    attachmentArn :: Types.String,
    -- | The status of the attachment.
    status :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachmentStateChange' value with any optional fields omitted.
mkAttachmentStateChange ::
  -- | 'attachmentArn'
  Types.String ->
  -- | 'status'
  Types.String ->
  AttachmentStateChange
mkAttachmentStateChange attachmentArn status =
  AttachmentStateChange' {attachmentArn, status}

-- | The Amazon Resource Name (ARN) of the attachment.
--
-- /Note:/ Consider using 'attachmentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascAttachmentArn :: Lens.Lens' AttachmentStateChange Types.String
ascAttachmentArn = Lens.field @"attachmentArn"
{-# DEPRECATED ascAttachmentArn "Use generic-lens or generic-optics with 'attachmentArn' instead." #-}

-- | The status of the attachment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascStatus :: Lens.Lens' AttachmentStateChange Types.String
ascStatus = Lens.field @"status"
{-# DEPRECATED ascStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AttachmentStateChange where
  toJSON AttachmentStateChange {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("attachmentArn" Core..= attachmentArn),
            Core.Just ("status" Core..= status)
          ]
      )
