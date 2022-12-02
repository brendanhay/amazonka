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
-- Module      : Amazonka.FraudDetector.Types.FieldValidationMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.FieldValidationMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The message details.
--
-- /See:/ 'newFieldValidationMessage' smart constructor.
data FieldValidationMessage = FieldValidationMessage'
  { -- | The message type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The field name.
    fieldName :: Prelude.Maybe Prelude.Text,
    -- | The message title.
    title :: Prelude.Maybe Prelude.Text,
    -- | The message ID.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The message content.
    content :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldValidationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'fieldValidationMessage_type' - The message type.
--
-- 'fieldName', 'fieldValidationMessage_fieldName' - The field name.
--
-- 'title', 'fieldValidationMessage_title' - The message title.
--
-- 'identifier', 'fieldValidationMessage_identifier' - The message ID.
--
-- 'content', 'fieldValidationMessage_content' - The message content.
newFieldValidationMessage ::
  FieldValidationMessage
newFieldValidationMessage =
  FieldValidationMessage'
    { type' = Prelude.Nothing,
      fieldName = Prelude.Nothing,
      title = Prelude.Nothing,
      identifier = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | The message type.
fieldValidationMessage_type :: Lens.Lens' FieldValidationMessage (Prelude.Maybe Prelude.Text)
fieldValidationMessage_type = Lens.lens (\FieldValidationMessage' {type'} -> type') (\s@FieldValidationMessage' {} a -> s {type' = a} :: FieldValidationMessage)

-- | The field name.
fieldValidationMessage_fieldName :: Lens.Lens' FieldValidationMessage (Prelude.Maybe Prelude.Text)
fieldValidationMessage_fieldName = Lens.lens (\FieldValidationMessage' {fieldName} -> fieldName) (\s@FieldValidationMessage' {} a -> s {fieldName = a} :: FieldValidationMessage)

-- | The message title.
fieldValidationMessage_title :: Lens.Lens' FieldValidationMessage (Prelude.Maybe Prelude.Text)
fieldValidationMessage_title = Lens.lens (\FieldValidationMessage' {title} -> title) (\s@FieldValidationMessage' {} a -> s {title = a} :: FieldValidationMessage)

-- | The message ID.
fieldValidationMessage_identifier :: Lens.Lens' FieldValidationMessage (Prelude.Maybe Prelude.Text)
fieldValidationMessage_identifier = Lens.lens (\FieldValidationMessage' {identifier} -> identifier) (\s@FieldValidationMessage' {} a -> s {identifier = a} :: FieldValidationMessage)

-- | The message content.
fieldValidationMessage_content :: Lens.Lens' FieldValidationMessage (Prelude.Maybe Prelude.Text)
fieldValidationMessage_content = Lens.lens (\FieldValidationMessage' {content} -> content) (\s@FieldValidationMessage' {} a -> s {content = a} :: FieldValidationMessage)

instance Data.FromJSON FieldValidationMessage where
  parseJSON =
    Data.withObject
      "FieldValidationMessage"
      ( \x ->
          FieldValidationMessage'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "fieldName")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "identifier")
            Prelude.<*> (x Data..:? "content")
      )

instance Prelude.Hashable FieldValidationMessage where
  hashWithSalt _salt FieldValidationMessage' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` content

instance Prelude.NFData FieldValidationMessage where
  rnf FieldValidationMessage' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf fieldName
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf content
