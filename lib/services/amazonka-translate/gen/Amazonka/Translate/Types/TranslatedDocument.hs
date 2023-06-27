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
-- Module      : Amazonka.Translate.Types.TranslatedDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TranslatedDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The translated content.
--
-- /See:/ 'newTranslatedDocument' smart constructor.
data TranslatedDocument = TranslatedDocument'
  { -- | The document containing the translated content.
    content :: Data.Sensitive Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslatedDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'translatedDocument_content' - The document containing the translated content.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newTranslatedDocument ::
  -- | 'content'
  Prelude.ByteString ->
  TranslatedDocument
newTranslatedDocument pContent_ =
  TranslatedDocument'
    { content =
        Data._Sensitive
          Prelude.. Data._Base64
          Lens.# pContent_
    }

-- | The document containing the translated content.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
translatedDocument_content :: Lens.Lens' TranslatedDocument Prelude.ByteString
translatedDocument_content = Lens.lens (\TranslatedDocument' {content} -> content) (\s@TranslatedDocument' {} a -> s {content = a} :: TranslatedDocument) Prelude.. Data._Sensitive Prelude.. Data._Base64

instance Data.FromJSON TranslatedDocument where
  parseJSON =
    Data.withObject
      "TranslatedDocument"
      ( \x ->
          TranslatedDocument'
            Prelude.<$> (x Data..: "Content")
      )

instance Prelude.Hashable TranslatedDocument where
  hashWithSalt _salt TranslatedDocument' {..} =
    _salt `Prelude.hashWithSalt` content

instance Prelude.NFData TranslatedDocument where
  rnf TranslatedDocument' {..} = Prelude.rnf content
