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
-- Module      : Amazonka.ConnectCases.GetCase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific case if it exists.
module Amazonka.ConnectCases.GetCase
  ( -- * Creating a Request
    GetCase (..),
    newGetCase,

    -- * Request Lenses
    getCase_nextToken,
    getCase_caseId,
    getCase_domainId,
    getCase_fields,

    -- * Destructuring the Response
    GetCaseResponse (..),
    newGetCaseResponse,

    -- * Response Lenses
    getCaseResponse_tags,
    getCaseResponse_nextToken,
    getCaseResponse_httpStatus,
    getCaseResponse_fields,
    getCaseResponse_templateId,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCase' smart constructor.
data GetCase = GetCase'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier of the case.
    caseId :: Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | A list of unique field identifiers.
    fields :: Prelude.NonEmpty FieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCase_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'caseId', 'getCase_caseId' - A unique identifier of the case.
--
-- 'domainId', 'getCase_domainId' - The unique identifier of the Cases domain.
--
-- 'fields', 'getCase_fields' - A list of unique field identifiers.
newGetCase ::
  -- | 'caseId'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fields'
  Prelude.NonEmpty FieldIdentifier ->
  GetCase
newGetCase pCaseId_ pDomainId_ pFields_ =
  GetCase'
    { nextToken = Prelude.Nothing,
      caseId = pCaseId_,
      domainId = pDomainId_,
      fields = Lens.coerced Lens.# pFields_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
getCase_nextToken :: Lens.Lens' GetCase (Prelude.Maybe Prelude.Text)
getCase_nextToken = Lens.lens (\GetCase' {nextToken} -> nextToken) (\s@GetCase' {} a -> s {nextToken = a} :: GetCase)

-- | A unique identifier of the case.
getCase_caseId :: Lens.Lens' GetCase Prelude.Text
getCase_caseId = Lens.lens (\GetCase' {caseId} -> caseId) (\s@GetCase' {} a -> s {caseId = a} :: GetCase)

-- | The unique identifier of the Cases domain.
getCase_domainId :: Lens.Lens' GetCase Prelude.Text
getCase_domainId = Lens.lens (\GetCase' {domainId} -> domainId) (\s@GetCase' {} a -> s {domainId = a} :: GetCase)

-- | A list of unique field identifiers.
getCase_fields :: Lens.Lens' GetCase (Prelude.NonEmpty FieldIdentifier)
getCase_fields = Lens.lens (\GetCase' {fields} -> fields) (\s@GetCase' {} a -> s {fields = a} :: GetCase) Prelude.. Lens.coerced

instance Core.AWSRequest GetCase where
  type AWSResponse GetCase = GetCaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCaseResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "fields" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "templateId")
      )

instance Prelude.Hashable GetCase where
  hashWithSalt _salt GetCase' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fields

instance Prelude.NFData GetCase where
  rnf GetCase' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fields

instance Data.ToHeaders GetCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCase where
  toJSON GetCase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("fields" Data..= fields)
          ]
      )

instance Data.ToPath GetCase where
  toPath GetCase' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/cases/",
        Data.toBS caseId
      ]

instance Data.ToQuery GetCase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCaseResponse' smart constructor.
data GetCaseResponse = GetCaseResponse'
  { -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of detailed field information.
    fields :: [FieldValue],
    -- | A unique identifier of a template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getCaseResponse_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'nextToken', 'getCaseResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'getCaseResponse_httpStatus' - The response's http status code.
--
-- 'fields', 'getCaseResponse_fields' - A list of detailed field information.
--
-- 'templateId', 'getCaseResponse_templateId' - A unique identifier of a template.
newGetCaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'templateId'
  Prelude.Text ->
  GetCaseResponse
newGetCaseResponse pHttpStatus_ pTemplateId_ =
  GetCaseResponse'
    { tags = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      fields = Prelude.mempty,
      templateId = pTemplateId_
    }

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
getCaseResponse_tags :: Lens.Lens' GetCaseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCaseResponse_tags = Lens.lens (\GetCaseResponse' {tags} -> tags) (\s@GetCaseResponse' {} a -> s {tags = a} :: GetCaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. This is null if there are no more
-- results to return.
getCaseResponse_nextToken :: Lens.Lens' GetCaseResponse (Prelude.Maybe Prelude.Text)
getCaseResponse_nextToken = Lens.lens (\GetCaseResponse' {nextToken} -> nextToken) (\s@GetCaseResponse' {} a -> s {nextToken = a} :: GetCaseResponse)

-- | The response's http status code.
getCaseResponse_httpStatus :: Lens.Lens' GetCaseResponse Prelude.Int
getCaseResponse_httpStatus = Lens.lens (\GetCaseResponse' {httpStatus} -> httpStatus) (\s@GetCaseResponse' {} a -> s {httpStatus = a} :: GetCaseResponse)

-- | A list of detailed field information.
getCaseResponse_fields :: Lens.Lens' GetCaseResponse [FieldValue]
getCaseResponse_fields = Lens.lens (\GetCaseResponse' {fields} -> fields) (\s@GetCaseResponse' {} a -> s {fields = a} :: GetCaseResponse) Prelude.. Lens.coerced

-- | A unique identifier of a template.
getCaseResponse_templateId :: Lens.Lens' GetCaseResponse Prelude.Text
getCaseResponse_templateId = Lens.lens (\GetCaseResponse' {templateId} -> templateId) (\s@GetCaseResponse' {} a -> s {templateId = a} :: GetCaseResponse)

instance Prelude.NFData GetCaseResponse where
  rnf GetCaseResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf templateId
