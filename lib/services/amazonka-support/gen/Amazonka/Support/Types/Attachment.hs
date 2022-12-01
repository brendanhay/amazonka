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
-- Module      : Amazonka.Support.Types.Attachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.Attachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An attachment to a case communication. The attachment consists of the
-- file name and the content of the file.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The name of the attachment file.
    fileName :: Prelude.Maybe Prelude.Text,
    -- | The content of the attachment file.
    data' :: Prelude.Maybe Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileName', 'attachment_fileName' - The name of the attachment file.
--
-- 'data'', 'attachment_data' - The content of the attachment file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { fileName = Prelude.Nothing,
      data' = Prelude.Nothing
    }

-- | The name of the attachment file.
attachment_fileName :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_fileName = Lens.lens (\Attachment' {fileName} -> fileName) (\s@Attachment' {} a -> s {fileName = a} :: Attachment)

-- | The content of the attachment file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
attachment_data :: Lens.Lens' Attachment (Prelude.Maybe Prelude.ByteString)
attachment_data = Lens.lens (\Attachment' {data'} -> data') (\s@Attachment' {} a -> s {data' = a} :: Attachment) Prelude.. Lens.mapping Core._Base64

instance Core.FromJSON Attachment where
  parseJSON =
    Core.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Core..:? "fileName")
            Prelude.<*> (x Core..:? "data")
      )

instance Prelude.Hashable Attachment where
  hashWithSalt _salt Attachment' {..} =
    _salt `Prelude.hashWithSalt` fileName
      `Prelude.hashWithSalt` data'

instance Prelude.NFData Attachment where
  rnf Attachment' {..} =
    Prelude.rnf fileName
      `Prelude.seq` Prelude.rnf data'

instance Core.ToJSON Attachment where
  toJSON Attachment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fileName" Core..=) Prelude.<$> fileName,
            ("data" Core..=) Prelude.<$> data'
          ]
      )
