{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AttachmentStateChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AttachmentStateChange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing a change in state for a task attachment.
--
-- /See:/ 'newAttachmentStateChange' smart constructor.
data AttachmentStateChange = AttachmentStateChange'
  { -- | The Amazon Resource Name (ARN) of the attachment.
    attachmentArn :: Core.Text,
    -- | The status of the attachment.
    status :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachmentStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentArn', 'attachmentStateChange_attachmentArn' - The Amazon Resource Name (ARN) of the attachment.
--
-- 'status', 'attachmentStateChange_status' - The status of the attachment.
newAttachmentStateChange ::
  -- | 'attachmentArn'
  Core.Text ->
  -- | 'status'
  Core.Text ->
  AttachmentStateChange
newAttachmentStateChange pAttachmentArn_ pStatus_ =
  AttachmentStateChange'
    { attachmentArn =
        pAttachmentArn_,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the attachment.
attachmentStateChange_attachmentArn :: Lens.Lens' AttachmentStateChange Core.Text
attachmentStateChange_attachmentArn = Lens.lens (\AttachmentStateChange' {attachmentArn} -> attachmentArn) (\s@AttachmentStateChange' {} a -> s {attachmentArn = a} :: AttachmentStateChange)

-- | The status of the attachment.
attachmentStateChange_status :: Lens.Lens' AttachmentStateChange Core.Text
attachmentStateChange_status = Lens.lens (\AttachmentStateChange' {status} -> status) (\s@AttachmentStateChange' {} a -> s {status = a} :: AttachmentStateChange)

instance Core.Hashable AttachmentStateChange

instance Core.NFData AttachmentStateChange

instance Core.ToJSON AttachmentStateChange where
  toJSON AttachmentStateChange' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("attachmentArn" Core..= attachmentArn),
            Core.Just ("status" Core..= status)
          ]
      )
