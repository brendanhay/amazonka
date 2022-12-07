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
-- Module      : Amazonka.FraudDetector.Types.FileValidationMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.FileValidationMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The message details.
--
-- /See:/ 'newFileValidationMessage' smart constructor.
data FileValidationMessage = FileValidationMessage'
  { -- | The message type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The message title.
    title :: Prelude.Maybe Prelude.Text,
    -- | The message content.
    content :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileValidationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'fileValidationMessage_type' - The message type.
--
-- 'title', 'fileValidationMessage_title' - The message title.
--
-- 'content', 'fileValidationMessage_content' - The message content.
newFileValidationMessage ::
  FileValidationMessage
newFileValidationMessage =
  FileValidationMessage'
    { type' = Prelude.Nothing,
      title = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | The message type.
fileValidationMessage_type :: Lens.Lens' FileValidationMessage (Prelude.Maybe Prelude.Text)
fileValidationMessage_type = Lens.lens (\FileValidationMessage' {type'} -> type') (\s@FileValidationMessage' {} a -> s {type' = a} :: FileValidationMessage)

-- | The message title.
fileValidationMessage_title :: Lens.Lens' FileValidationMessage (Prelude.Maybe Prelude.Text)
fileValidationMessage_title = Lens.lens (\FileValidationMessage' {title} -> title) (\s@FileValidationMessage' {} a -> s {title = a} :: FileValidationMessage)

-- | The message content.
fileValidationMessage_content :: Lens.Lens' FileValidationMessage (Prelude.Maybe Prelude.Text)
fileValidationMessage_content = Lens.lens (\FileValidationMessage' {content} -> content) (\s@FileValidationMessage' {} a -> s {content = a} :: FileValidationMessage)

instance Data.FromJSON FileValidationMessage where
  parseJSON =
    Data.withObject
      "FileValidationMessage"
      ( \x ->
          FileValidationMessage'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "content")
      )

instance Prelude.Hashable FileValidationMessage where
  hashWithSalt _salt FileValidationMessage' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` content

instance Prelude.NFData FileValidationMessage where
  rnf FileValidationMessage' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf content
