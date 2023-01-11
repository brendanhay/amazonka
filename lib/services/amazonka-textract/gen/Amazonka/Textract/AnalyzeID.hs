{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Textract.AnalyzeID
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Analyzes identity documents for relevant information. This information
-- is extracted and returned as @IdentityDocumentFields@, which records
-- both the normalized field and value of the extracted text.Unlike other
-- Amazon Textract operations, @AnalyzeID@ doesn\'t return any Geometry
-- data.
module Amazonka.Textract.AnalyzeID
  ( -- * Creating a Request
    AnalyzeID (..),
    newAnalyzeID,

    -- * Request Lenses
    analyzeID_documentPages,

    -- * Destructuring the Response
    AnalyzeIDResponse (..),
    newAnalyzeIDResponse,

    -- * Response Lenses
    analyzeIDResponse_analyzeIDModelVersion,
    analyzeIDResponse_documentMetadata,
    analyzeIDResponse_identityDocuments,
    analyzeIDResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newAnalyzeID' smart constructor.
data AnalyzeID = AnalyzeID'
  { -- | The document being passed to AnalyzeID.
    documentPages :: Prelude.NonEmpty Document
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeID' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentPages', 'analyzeID_documentPages' - The document being passed to AnalyzeID.
newAnalyzeID ::
  -- | 'documentPages'
  Prelude.NonEmpty Document ->
  AnalyzeID
newAnalyzeID pDocumentPages_ =
  AnalyzeID'
    { documentPages =
        Lens.coerced Lens.# pDocumentPages_
    }

-- | The document being passed to AnalyzeID.
analyzeID_documentPages :: Lens.Lens' AnalyzeID (Prelude.NonEmpty Document)
analyzeID_documentPages = Lens.lens (\AnalyzeID' {documentPages} -> documentPages) (\s@AnalyzeID' {} a -> s {documentPages = a} :: AnalyzeID) Prelude.. Lens.coerced

instance Core.AWSRequest AnalyzeID where
  type AWSResponse AnalyzeID = AnalyzeIDResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AnalyzeIDResponse'
            Prelude.<$> (x Data..?> "AnalyzeIDModelVersion")
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> ( x Data..?> "IdentityDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AnalyzeID where
  hashWithSalt _salt AnalyzeID' {..} =
    _salt `Prelude.hashWithSalt` documentPages

instance Prelude.NFData AnalyzeID where
  rnf AnalyzeID' {..} = Prelude.rnf documentPages

instance Data.ToHeaders AnalyzeID where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Textract.AnalyzeID" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AnalyzeID where
  toJSON AnalyzeID' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DocumentPages" Data..= documentPages)
          ]
      )

instance Data.ToPath AnalyzeID where
  toPath = Prelude.const "/"

instance Data.ToQuery AnalyzeID where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAnalyzeIDResponse' smart constructor.
data AnalyzeIDResponse = AnalyzeIDResponse'
  { -- | The version of the AnalyzeIdentity API being used to process documents.
    analyzeIDModelVersion :: Prelude.Maybe Prelude.Text,
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The list of documents processed by AnalyzeID. Includes a number denoting
    -- their place in the list and the response structure for the document.
    identityDocuments :: Prelude.Maybe [IdentityDocument],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeIDResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzeIDModelVersion', 'analyzeIDResponse_analyzeIDModelVersion' - The version of the AnalyzeIdentity API being used to process documents.
--
-- 'documentMetadata', 'analyzeIDResponse_documentMetadata' - Undocumented member.
--
-- 'identityDocuments', 'analyzeIDResponse_identityDocuments' - The list of documents processed by AnalyzeID. Includes a number denoting
-- their place in the list and the response structure for the document.
--
-- 'httpStatus', 'analyzeIDResponse_httpStatus' - The response's http status code.
newAnalyzeIDResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AnalyzeIDResponse
newAnalyzeIDResponse pHttpStatus_ =
  AnalyzeIDResponse'
    { analyzeIDModelVersion =
        Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      identityDocuments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the AnalyzeIdentity API being used to process documents.
analyzeIDResponse_analyzeIDModelVersion :: Lens.Lens' AnalyzeIDResponse (Prelude.Maybe Prelude.Text)
analyzeIDResponse_analyzeIDModelVersion = Lens.lens (\AnalyzeIDResponse' {analyzeIDModelVersion} -> analyzeIDModelVersion) (\s@AnalyzeIDResponse' {} a -> s {analyzeIDModelVersion = a} :: AnalyzeIDResponse)

-- | Undocumented member.
analyzeIDResponse_documentMetadata :: Lens.Lens' AnalyzeIDResponse (Prelude.Maybe DocumentMetadata)
analyzeIDResponse_documentMetadata = Lens.lens (\AnalyzeIDResponse' {documentMetadata} -> documentMetadata) (\s@AnalyzeIDResponse' {} a -> s {documentMetadata = a} :: AnalyzeIDResponse)

-- | The list of documents processed by AnalyzeID. Includes a number denoting
-- their place in the list and the response structure for the document.
analyzeIDResponse_identityDocuments :: Lens.Lens' AnalyzeIDResponse (Prelude.Maybe [IdentityDocument])
analyzeIDResponse_identityDocuments = Lens.lens (\AnalyzeIDResponse' {identityDocuments} -> identityDocuments) (\s@AnalyzeIDResponse' {} a -> s {identityDocuments = a} :: AnalyzeIDResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
analyzeIDResponse_httpStatus :: Lens.Lens' AnalyzeIDResponse Prelude.Int
analyzeIDResponse_httpStatus = Lens.lens (\AnalyzeIDResponse' {httpStatus} -> httpStatus) (\s@AnalyzeIDResponse' {} a -> s {httpStatus = a} :: AnalyzeIDResponse)

instance Prelude.NFData AnalyzeIDResponse where
  rnf AnalyzeIDResponse' {..} =
    Prelude.rnf analyzeIDModelVersion
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf identityDocuments
      `Prelude.seq` Prelude.rnf httpStatus
