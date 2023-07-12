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
-- Module      : Amazonka.ECS.Types.AttachmentStateChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.AttachmentStateChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a change in state for a task attachment.
--
-- /See:/ 'newAttachmentStateChange' smart constructor.
data AttachmentStateChange = AttachmentStateChange'
  { -- | The Amazon Resource Name (ARN) of the attachment.
    attachmentArn :: Prelude.Text,
    -- | The status of the attachment.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  AttachmentStateChange
newAttachmentStateChange pAttachmentArn_ pStatus_ =
  AttachmentStateChange'
    { attachmentArn =
        pAttachmentArn_,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the attachment.
attachmentStateChange_attachmentArn :: Lens.Lens' AttachmentStateChange Prelude.Text
attachmentStateChange_attachmentArn = Lens.lens (\AttachmentStateChange' {attachmentArn} -> attachmentArn) (\s@AttachmentStateChange' {} a -> s {attachmentArn = a} :: AttachmentStateChange)

-- | The status of the attachment.
attachmentStateChange_status :: Lens.Lens' AttachmentStateChange Prelude.Text
attachmentStateChange_status = Lens.lens (\AttachmentStateChange' {status} -> status) (\s@AttachmentStateChange' {} a -> s {status = a} :: AttachmentStateChange)

instance Prelude.Hashable AttachmentStateChange where
  hashWithSalt _salt AttachmentStateChange' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData AttachmentStateChange where
  rnf AttachmentStateChange' {..} =
    Prelude.rnf attachmentArn
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AttachmentStateChange where
  toJSON AttachmentStateChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("attachmentArn" Data..= attachmentArn),
            Prelude.Just ("status" Data..= status)
          ]
      )
