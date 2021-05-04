{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An attribute of an attachment, such as the attachment name.
--
-- /See:/ 'newAttachmentInformation' smart constructor.
data AttachmentInformation = AttachmentInformation'
  { -- | The name of the attachment.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  AttachmentInformation' {name = Prelude.Nothing}

-- | The name of the attachment.
attachmentInformation_name :: Lens.Lens' AttachmentInformation (Prelude.Maybe Prelude.Text)
attachmentInformation_name = Lens.lens (\AttachmentInformation' {name} -> name) (\s@AttachmentInformation' {} a -> s {name = a} :: AttachmentInformation)

instance Prelude.FromJSON AttachmentInformation where
  parseJSON =
    Prelude.withObject
      "AttachmentInformation"
      ( \x ->
          AttachmentInformation'
            Prelude.<$> (x Prelude..:? "Name")
      )

instance Prelude.Hashable AttachmentInformation

instance Prelude.NFData AttachmentInformation
