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
-- Module      : Amazonka.Connect.Types.AttachmentReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AttachmentReference where

import Amazonka.Connect.Types.ReferenceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a reference when the @referenceType@ is @ATTACHMENT@.
-- Otherwise, null.
--
-- /See:/ 'newAttachmentReference' smart constructor.
data AttachmentReference = AttachmentReference'
  { -- | Identifier of the attachment reference.
    name :: Prelude.Maybe Prelude.Text,
    -- | Status of the attachment reference type.
    status :: Prelude.Maybe ReferenceStatus,
    -- | The location path of the attachment reference.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachmentReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'attachmentReference_name' - Identifier of the attachment reference.
--
-- 'status', 'attachmentReference_status' - Status of the attachment reference type.
--
-- 'value', 'attachmentReference_value' - The location path of the attachment reference.
newAttachmentReference ::
  AttachmentReference
newAttachmentReference =
  AttachmentReference'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Identifier of the attachment reference.
attachmentReference_name :: Lens.Lens' AttachmentReference (Prelude.Maybe Prelude.Text)
attachmentReference_name = Lens.lens (\AttachmentReference' {name} -> name) (\s@AttachmentReference' {} a -> s {name = a} :: AttachmentReference)

-- | Status of the attachment reference type.
attachmentReference_status :: Lens.Lens' AttachmentReference (Prelude.Maybe ReferenceStatus)
attachmentReference_status = Lens.lens (\AttachmentReference' {status} -> status) (\s@AttachmentReference' {} a -> s {status = a} :: AttachmentReference)

-- | The location path of the attachment reference.
attachmentReference_value :: Lens.Lens' AttachmentReference (Prelude.Maybe Prelude.Text)
attachmentReference_value = Lens.lens (\AttachmentReference' {value} -> value) (\s@AttachmentReference' {} a -> s {value = a} :: AttachmentReference)

instance Core.FromJSON AttachmentReference where
  parseJSON =
    Core.withObject
      "AttachmentReference"
      ( \x ->
          AttachmentReference'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable AttachmentReference where
  hashWithSalt _salt AttachmentReference' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` value

instance Prelude.NFData AttachmentReference where
  rnf AttachmentReference' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf value
