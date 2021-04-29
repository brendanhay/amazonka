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
-- Module      : Network.AWS.Support.Types.Attachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Attachment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An attachment to a case communication. The attachment consists of the
-- file name and the content of the file.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The content of the attachment file.
    data' :: Prelude.Maybe Prelude.Base64,
    -- | The name of the attachment file.
    fileName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Attachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'attachment_data' - The content of the attachment file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'fileName', 'attachment_fileName' - The name of the attachment file.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { data' = Prelude.Nothing,
      fileName = Prelude.Nothing
    }

-- | The content of the attachment file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
attachment_data :: Lens.Lens' Attachment (Prelude.Maybe Prelude.ByteString)
attachment_data = Lens.lens (\Attachment' {data'} -> data') (\s@Attachment' {} a -> s {data' = a} :: Attachment) Prelude.. Lens.mapping Prelude._Base64

-- | The name of the attachment file.
attachment_fileName :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_fileName = Lens.lens (\Attachment' {fileName} -> fileName) (\s@Attachment' {} a -> s {fileName = a} :: Attachment)

instance Prelude.FromJSON Attachment where
  parseJSON =
    Prelude.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Prelude..:? "data")
            Prelude.<*> (x Prelude..:? "fileName")
      )

instance Prelude.Hashable Attachment

instance Prelude.NFData Attachment

instance Prelude.ToJSON Attachment where
  toJSON Attachment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("data" Prelude..=) Prelude.<$> data',
            ("fileName" Prelude..=) Prelude.<$> fileName
          ]
      )
