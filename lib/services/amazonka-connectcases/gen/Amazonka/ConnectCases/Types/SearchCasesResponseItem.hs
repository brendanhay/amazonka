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
-- Module      : Amazonka.ConnectCases.Types.SearchCasesResponseItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.SearchCasesResponseItem where

import Amazonka.ConnectCases.Types.FieldValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of items that represent cases.
--
-- /See:/ 'newSearchCasesResponseItem' smart constructor.
data SearchCasesResponseItem = SearchCasesResponseItem'
  { -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier of the case.
    caseId :: Prelude.Text,
    -- | List of case field values.
    fields :: [FieldValue],
    -- | A unique identifier of a template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchCasesResponseItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'searchCasesResponseItem_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'caseId', 'searchCasesResponseItem_caseId' - A unique identifier of the case.
--
-- 'fields', 'searchCasesResponseItem_fields' - List of case field values.
--
-- 'templateId', 'searchCasesResponseItem_templateId' - A unique identifier of a template.
newSearchCasesResponseItem ::
  -- | 'caseId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  SearchCasesResponseItem
newSearchCasesResponseItem pCaseId_ pTemplateId_ =
  SearchCasesResponseItem'
    { tags = Prelude.Nothing,
      caseId = pCaseId_,
      fields = Prelude.mempty,
      templateId = pTemplateId_
    }

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
searchCasesResponseItem_tags :: Lens.Lens' SearchCasesResponseItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
searchCasesResponseItem_tags = Lens.lens (\SearchCasesResponseItem' {tags} -> tags) (\s@SearchCasesResponseItem' {} a -> s {tags = a} :: SearchCasesResponseItem) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier of the case.
searchCasesResponseItem_caseId :: Lens.Lens' SearchCasesResponseItem Prelude.Text
searchCasesResponseItem_caseId = Lens.lens (\SearchCasesResponseItem' {caseId} -> caseId) (\s@SearchCasesResponseItem' {} a -> s {caseId = a} :: SearchCasesResponseItem)

-- | List of case field values.
searchCasesResponseItem_fields :: Lens.Lens' SearchCasesResponseItem [FieldValue]
searchCasesResponseItem_fields = Lens.lens (\SearchCasesResponseItem' {fields} -> fields) (\s@SearchCasesResponseItem' {} a -> s {fields = a} :: SearchCasesResponseItem) Prelude.. Lens.coerced

-- | A unique identifier of a template.
searchCasesResponseItem_templateId :: Lens.Lens' SearchCasesResponseItem Prelude.Text
searchCasesResponseItem_templateId = Lens.lens (\SearchCasesResponseItem' {templateId} -> templateId) (\s@SearchCasesResponseItem' {} a -> s {templateId = a} :: SearchCasesResponseItem)

instance Data.FromJSON SearchCasesResponseItem where
  parseJSON =
    Data.withObject
      "SearchCasesResponseItem"
      ( \x ->
          SearchCasesResponseItem'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "caseId")
            Prelude.<*> (x Data..:? "fields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "templateId")
      )

instance Prelude.Hashable SearchCasesResponseItem where
  hashWithSalt _salt SearchCasesResponseItem' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData SearchCasesResponseItem where
  rnf SearchCasesResponseItem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf templateId
