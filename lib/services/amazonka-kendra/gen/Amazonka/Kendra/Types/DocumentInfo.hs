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
-- Module      : Amazonka.Kendra.Types.DocumentInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttribute
import qualified Amazonka.Prelude as Prelude

-- | Identifies a document for which to retrieve status information
--
-- /See:/ 'newDocumentInfo' smart constructor.
data DocumentInfo = DocumentInfo'
  { -- | Attributes that identify a specific version of a document to check.
    --
    -- The only valid attributes are:
    --
    -- -   version
    --
    -- -   datasourceId
    --
    -- -   jobExecutionId
    --
    -- The attributes follow these rules:
    --
    -- -   @dataSourceId@ and @jobExecutionId@ must be used together.
    --
    -- -   @version@ is ignored if @dataSourceId@ and @jobExecutionId@ are not
    --     provided.
    --
    -- -   If @dataSourceId@ and @jobExecutionId@ are provided, but @version@
    --     is not, the version defaults to \"0\".
    attributes :: Prelude.Maybe [DocumentAttribute],
    -- | The identifier of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'documentInfo_attributes' - Attributes that identify a specific version of a document to check.
--
-- The only valid attributes are:
--
-- -   version
--
-- -   datasourceId
--
-- -   jobExecutionId
--
-- The attributes follow these rules:
--
-- -   @dataSourceId@ and @jobExecutionId@ must be used together.
--
-- -   @version@ is ignored if @dataSourceId@ and @jobExecutionId@ are not
--     provided.
--
-- -   If @dataSourceId@ and @jobExecutionId@ are provided, but @version@
--     is not, the version defaults to \"0\".
--
-- 'documentId', 'documentInfo_documentId' - The identifier of the document.
newDocumentInfo ::
  -- | 'documentId'
  Prelude.Text ->
  DocumentInfo
newDocumentInfo pDocumentId_ =
  DocumentInfo'
    { attributes = Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | Attributes that identify a specific version of a document to check.
--
-- The only valid attributes are:
--
-- -   version
--
-- -   datasourceId
--
-- -   jobExecutionId
--
-- The attributes follow these rules:
--
-- -   @dataSourceId@ and @jobExecutionId@ must be used together.
--
-- -   @version@ is ignored if @dataSourceId@ and @jobExecutionId@ are not
--     provided.
--
-- -   If @dataSourceId@ and @jobExecutionId@ are provided, but @version@
--     is not, the version defaults to \"0\".
documentInfo_attributes :: Lens.Lens' DocumentInfo (Prelude.Maybe [DocumentAttribute])
documentInfo_attributes = Lens.lens (\DocumentInfo' {attributes} -> attributes) (\s@DocumentInfo' {} a -> s {attributes = a} :: DocumentInfo) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the document.
documentInfo_documentId :: Lens.Lens' DocumentInfo Prelude.Text
documentInfo_documentId = Lens.lens (\DocumentInfo' {documentId} -> documentId) (\s@DocumentInfo' {} a -> s {documentId = a} :: DocumentInfo)

instance Prelude.Hashable DocumentInfo where
  hashWithSalt _salt DocumentInfo' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` documentId

instance Prelude.NFData DocumentInfo where
  rnf DocumentInfo' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf documentId

instance Data.ToJSON DocumentInfo where
  toJSON DocumentInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            Prelude.Just ("DocumentId" Data..= documentId)
          ]
      )
