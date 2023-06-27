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
-- Module      : Amazonka.Kendra.Types.SourceDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SourceDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttribute
import qualified Amazonka.Prelude as Prelude

-- | The document ID and its fields\/attributes that are used for a query
-- suggestion, if document fields set to use for query suggestions.
--
-- /See:/ 'newSourceDocument' smart constructor.
data SourceDocument = SourceDocument'
  { -- | The additional fields\/attributes to include in the response. You can
    -- use additional fields to provide extra information in the response.
    -- Additional fields are not used to based suggestions on.
    additionalAttributes :: Prelude.Maybe [DocumentAttribute],
    -- | The identifier of the document used for a query suggestion.
    documentId :: Prelude.Maybe Prelude.Text,
    -- | The document fields\/attributes used for a query suggestion.
    suggestionAttributes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAttributes', 'sourceDocument_additionalAttributes' - The additional fields\/attributes to include in the response. You can
-- use additional fields to provide extra information in the response.
-- Additional fields are not used to based suggestions on.
--
-- 'documentId', 'sourceDocument_documentId' - The identifier of the document used for a query suggestion.
--
-- 'suggestionAttributes', 'sourceDocument_suggestionAttributes' - The document fields\/attributes used for a query suggestion.
newSourceDocument ::
  SourceDocument
newSourceDocument =
  SourceDocument'
    { additionalAttributes =
        Prelude.Nothing,
      documentId = Prelude.Nothing,
      suggestionAttributes = Prelude.Nothing
    }

-- | The additional fields\/attributes to include in the response. You can
-- use additional fields to provide extra information in the response.
-- Additional fields are not used to based suggestions on.
sourceDocument_additionalAttributes :: Lens.Lens' SourceDocument (Prelude.Maybe [DocumentAttribute])
sourceDocument_additionalAttributes = Lens.lens (\SourceDocument' {additionalAttributes} -> additionalAttributes) (\s@SourceDocument' {} a -> s {additionalAttributes = a} :: SourceDocument) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the document used for a query suggestion.
sourceDocument_documentId :: Lens.Lens' SourceDocument (Prelude.Maybe Prelude.Text)
sourceDocument_documentId = Lens.lens (\SourceDocument' {documentId} -> documentId) (\s@SourceDocument' {} a -> s {documentId = a} :: SourceDocument)

-- | The document fields\/attributes used for a query suggestion.
sourceDocument_suggestionAttributes :: Lens.Lens' SourceDocument (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
sourceDocument_suggestionAttributes = Lens.lens (\SourceDocument' {suggestionAttributes} -> suggestionAttributes) (\s@SourceDocument' {} a -> s {suggestionAttributes = a} :: SourceDocument) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SourceDocument where
  parseJSON =
    Data.withObject
      "SourceDocument"
      ( \x ->
          SourceDocument'
            Prelude.<$> ( x
                            Data..:? "AdditionalAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DocumentId")
            Prelude.<*> (x Data..:? "SuggestionAttributes")
      )

instance Prelude.Hashable SourceDocument where
  hashWithSalt _salt SourceDocument' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAttributes
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` suggestionAttributes

instance Prelude.NFData SourceDocument where
  rnf SourceDocument' {..} =
    Prelude.rnf additionalAttributes
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf suggestionAttributes
