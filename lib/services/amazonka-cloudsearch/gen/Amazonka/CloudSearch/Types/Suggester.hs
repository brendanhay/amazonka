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
-- Module      : Amazonka.CloudSearch.Types.Suggester
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.Suggester where

import Amazonka.CloudSearch.Types.DocumentSuggesterOptions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for
-- suggestions. The following options can be configured for a suggester:
-- @FuzzyMatching@, @SortExpression@.
--
-- /See:/ 'newSuggester' smart constructor.
data Suggester = Suggester'
  { suggesterName :: Prelude.Text,
    documentSuggesterOptions :: DocumentSuggesterOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Suggester' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggesterName', 'suggester_suggesterName' - Undocumented member.
--
-- 'documentSuggesterOptions', 'suggester_documentSuggesterOptions' - Undocumented member.
newSuggester ::
  -- | 'suggesterName'
  Prelude.Text ->
  -- | 'documentSuggesterOptions'
  DocumentSuggesterOptions ->
  Suggester
newSuggester
  pSuggesterName_
  pDocumentSuggesterOptions_ =
    Suggester'
      { suggesterName = pSuggesterName_,
        documentSuggesterOptions =
          pDocumentSuggesterOptions_
      }

-- | Undocumented member.
suggester_suggesterName :: Lens.Lens' Suggester Prelude.Text
suggester_suggesterName = Lens.lens (\Suggester' {suggesterName} -> suggesterName) (\s@Suggester' {} a -> s {suggesterName = a} :: Suggester)

-- | Undocumented member.
suggester_documentSuggesterOptions :: Lens.Lens' Suggester DocumentSuggesterOptions
suggester_documentSuggesterOptions = Lens.lens (\Suggester' {documentSuggesterOptions} -> documentSuggesterOptions) (\s@Suggester' {} a -> s {documentSuggesterOptions = a} :: Suggester)

instance Data.FromXML Suggester where
  parseXML x =
    Suggester'
      Prelude.<$> (x Data..@ "SuggesterName")
      Prelude.<*> (x Data..@ "DocumentSuggesterOptions")

instance Prelude.Hashable Suggester where
  hashWithSalt _salt Suggester' {..} =
    _salt `Prelude.hashWithSalt` suggesterName
      `Prelude.hashWithSalt` documentSuggesterOptions

instance Prelude.NFData Suggester where
  rnf Suggester' {..} =
    Prelude.rnf suggesterName
      `Prelude.seq` Prelude.rnf documentSuggesterOptions

instance Data.ToQuery Suggester where
  toQuery Suggester' {..} =
    Prelude.mconcat
      [ "SuggesterName" Data.=: suggesterName,
        "DocumentSuggesterOptions"
          Data.=: documentSuggesterOptions
      ]
