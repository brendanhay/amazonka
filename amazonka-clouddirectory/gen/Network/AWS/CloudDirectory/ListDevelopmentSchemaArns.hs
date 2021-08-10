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
-- Module      : Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each Amazon Resource Name (ARN) of schemas in the development
-- state.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
  ( -- * Creating a Request
    ListDevelopmentSchemaArns (..),
    newListDevelopmentSchemaArns,

    -- * Request Lenses
    listDevelopmentSchemaArns_nextToken,
    listDevelopmentSchemaArns_maxResults,

    -- * Destructuring the Response
    ListDevelopmentSchemaArnsResponse (..),
    newListDevelopmentSchemaArnsResponse,

    -- * Response Lenses
    listDevelopmentSchemaArnsResponse_schemaArns,
    listDevelopmentSchemaArnsResponse_nextToken,
    listDevelopmentSchemaArnsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDevelopmentSchemaArns' smart constructor.
data ListDevelopmentSchemaArns = ListDevelopmentSchemaArns'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevelopmentSchemaArns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevelopmentSchemaArns_nextToken' - The pagination token.
--
-- 'maxResults', 'listDevelopmentSchemaArns_maxResults' - The maximum number of results to retrieve.
newListDevelopmentSchemaArns ::
  ListDevelopmentSchemaArns
newListDevelopmentSchemaArns =
  ListDevelopmentSchemaArns'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token.
listDevelopmentSchemaArns_nextToken :: Lens.Lens' ListDevelopmentSchemaArns (Prelude.Maybe Prelude.Text)
listDevelopmentSchemaArns_nextToken = Lens.lens (\ListDevelopmentSchemaArns' {nextToken} -> nextToken) (\s@ListDevelopmentSchemaArns' {} a -> s {nextToken = a} :: ListDevelopmentSchemaArns)

-- | The maximum number of results to retrieve.
listDevelopmentSchemaArns_maxResults :: Lens.Lens' ListDevelopmentSchemaArns (Prelude.Maybe Prelude.Natural)
listDevelopmentSchemaArns_maxResults = Lens.lens (\ListDevelopmentSchemaArns' {maxResults} -> maxResults) (\s@ListDevelopmentSchemaArns' {} a -> s {maxResults = a} :: ListDevelopmentSchemaArns)

instance Core.AWSPager ListDevelopmentSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevelopmentSchemaArnsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevelopmentSchemaArnsResponse_schemaArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDevelopmentSchemaArns_nextToken
          Lens..~ rs
          Lens.^? listDevelopmentSchemaArnsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDevelopmentSchemaArns where
  type
    AWSResponse ListDevelopmentSchemaArns =
      ListDevelopmentSchemaArnsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevelopmentSchemaArnsResponse'
            Prelude.<$> (x Core..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevelopmentSchemaArns

instance Prelude.NFData ListDevelopmentSchemaArns

instance Core.ToHeaders ListDevelopmentSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListDevelopmentSchemaArns where
  toJSON ListDevelopmentSchemaArns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDevelopmentSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/development"

instance Core.ToQuery ListDevelopmentSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDevelopmentSchemaArnsResponse' smart constructor.
data ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse'
  { -- | The ARNs of retrieved development schemas.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevelopmentSchemaArnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArns', 'listDevelopmentSchemaArnsResponse_schemaArns' - The ARNs of retrieved development schemas.
--
-- 'nextToken', 'listDevelopmentSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listDevelopmentSchemaArnsResponse_httpStatus' - The response's http status code.
newListDevelopmentSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevelopmentSchemaArnsResponse
newListDevelopmentSchemaArnsResponse pHttpStatus_ =
  ListDevelopmentSchemaArnsResponse'
    { schemaArns =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs of retrieved development schemas.
listDevelopmentSchemaArnsResponse_schemaArns :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listDevelopmentSchemaArnsResponse_schemaArns = Lens.lens (\ListDevelopmentSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListDevelopmentSchemaArnsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token.
listDevelopmentSchemaArnsResponse_nextToken :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listDevelopmentSchemaArnsResponse_nextToken = Lens.lens (\ListDevelopmentSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {nextToken = a} :: ListDevelopmentSchemaArnsResponse)

-- | The response's http status code.
listDevelopmentSchemaArnsResponse_httpStatus :: Lens.Lens' ListDevelopmentSchemaArnsResponse Prelude.Int
listDevelopmentSchemaArnsResponse_httpStatus = Lens.lens (\ListDevelopmentSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListDevelopmentSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListDevelopmentSchemaArnsResponse)

instance
  Prelude.NFData
    ListDevelopmentSchemaArnsResponse
