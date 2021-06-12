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
-- Module      : Network.AWS.SSM.Types.AttachmentInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentInformation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An attribute of an attachment, such as the attachment name.
--
-- /See:/ 'newAttachmentInformation' smart constructor.
data AttachmentInformation = AttachmentInformation'
  { -- | The name of the attachment.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachmentInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'attachmentInformation_name' - The name of the attachment.
newAttachmentInformation ::
  AttachmentInformation
newAttachmentInformation =
  AttachmentInformation' {name = Core.Nothing}

-- | The name of the attachment.
attachmentInformation_name :: Lens.Lens' AttachmentInformation (Core.Maybe Core.Text)
attachmentInformation_name = Lens.lens (\AttachmentInformation' {name} -> name) (\s@AttachmentInformation' {} a -> s {name = a} :: AttachmentInformation)

instance Core.FromJSON AttachmentInformation where
  parseJSON =
    Core.withObject
      "AttachmentInformation"
      ( \x ->
          AttachmentInformation' Core.<$> (x Core..:? "Name")
      )

instance Core.Hashable AttachmentInformation

instance Core.NFData AttachmentInformation
