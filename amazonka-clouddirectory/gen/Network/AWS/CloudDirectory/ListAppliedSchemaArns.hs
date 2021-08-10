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
-- Module      : Network.AWS.CloudDirectory.ListAppliedSchemaArns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists schema major versions applied to a directory. If @SchemaArn@ is
-- provided, lists the minor version.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAppliedSchemaArns
  ( -- * Creating a Request
    ListAppliedSchemaArns (..),
    newListAppliedSchemaArns,

    -- * Request Lenses
    listAppliedSchemaArns_schemaArn,
    listAppliedSchemaArns_nextToken,
    listAppliedSchemaArns_maxResults,
    listAppliedSchemaArns_directoryArn,

    -- * Destructuring the Response
    ListAppliedSchemaArnsResponse (..),
    newListAppliedSchemaArnsResponse,

    -- * Response Lenses
    listAppliedSchemaArnsResponse_schemaArns,
    listAppliedSchemaArnsResponse_nextToken,
    listAppliedSchemaArnsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAppliedSchemaArns' smart constructor.
data ListAppliedSchemaArns = ListAppliedSchemaArns'
  { -- | The response for @ListAppliedSchemaArns@ when this parameter is used
    -- will list all minor version ARNs for a major version.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the directory you are listing.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppliedSchemaArns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'listAppliedSchemaArns_schemaArn' - The response for @ListAppliedSchemaArns@ when this parameter is used
-- will list all minor version ARNs for a major version.
--
-- 'nextToken', 'listAppliedSchemaArns_nextToken' - The pagination token.
--
-- 'maxResults', 'listAppliedSchemaArns_maxResults' - The maximum number of results to retrieve.
--
-- 'directoryArn', 'listAppliedSchemaArns_directoryArn' - The ARN of the directory you are listing.
newListAppliedSchemaArns ::
  -- | 'directoryArn'
  Prelude.Text ->
  ListAppliedSchemaArns
newListAppliedSchemaArns pDirectoryArn_ =
  ListAppliedSchemaArns'
    { schemaArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      directoryArn = pDirectoryArn_
    }

-- | The response for @ListAppliedSchemaArns@ when this parameter is used
-- will list all minor version ARNs for a major version.
listAppliedSchemaArns_schemaArn :: Lens.Lens' ListAppliedSchemaArns (Prelude.Maybe Prelude.Text)
listAppliedSchemaArns_schemaArn = Lens.lens (\ListAppliedSchemaArns' {schemaArn} -> schemaArn) (\s@ListAppliedSchemaArns' {} a -> s {schemaArn = a} :: ListAppliedSchemaArns)

-- | The pagination token.
listAppliedSchemaArns_nextToken :: Lens.Lens' ListAppliedSchemaArns (Prelude.Maybe Prelude.Text)
listAppliedSchemaArns_nextToken = Lens.lens (\ListAppliedSchemaArns' {nextToken} -> nextToken) (\s@ListAppliedSchemaArns' {} a -> s {nextToken = a} :: ListAppliedSchemaArns)

-- | The maximum number of results to retrieve.
listAppliedSchemaArns_maxResults :: Lens.Lens' ListAppliedSchemaArns (Prelude.Maybe Prelude.Natural)
listAppliedSchemaArns_maxResults = Lens.lens (\ListAppliedSchemaArns' {maxResults} -> maxResults) (\s@ListAppliedSchemaArns' {} a -> s {maxResults = a} :: ListAppliedSchemaArns)

-- | The ARN of the directory you are listing.
listAppliedSchemaArns_directoryArn :: Lens.Lens' ListAppliedSchemaArns Prelude.Text
listAppliedSchemaArns_directoryArn = Lens.lens (\ListAppliedSchemaArns' {directoryArn} -> directoryArn) (\s@ListAppliedSchemaArns' {} a -> s {directoryArn = a} :: ListAppliedSchemaArns)

instance Core.AWSPager ListAppliedSchemaArns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppliedSchemaArnsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppliedSchemaArnsResponse_schemaArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAppliedSchemaArns_nextToken
          Lens..~ rs
          Lens.^? listAppliedSchemaArnsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAppliedSchemaArns where
  type
    AWSResponse ListAppliedSchemaArns =
      ListAppliedSchemaArnsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppliedSchemaArnsResponse'
            Prelude.<$> (x Core..?> "SchemaArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppliedSchemaArns

instance Prelude.NFData ListAppliedSchemaArns

instance Core.ToHeaders ListAppliedSchemaArns where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListAppliedSchemaArns where
  toJSON ListAppliedSchemaArns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaArn" Core..=) Prelude.<$> schemaArn,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DirectoryArn" Core..= directoryArn)
          ]
      )

instance Core.ToPath ListAppliedSchemaArns where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/applied"

instance Core.ToQuery ListAppliedSchemaArns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppliedSchemaArnsResponse' smart constructor.
data ListAppliedSchemaArnsResponse = ListAppliedSchemaArnsResponse'
  { -- | The ARNs of schemas that are applied to the directory.
    schemaArns :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppliedSchemaArnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArns', 'listAppliedSchemaArnsResponse_schemaArns' - The ARNs of schemas that are applied to the directory.
--
-- 'nextToken', 'listAppliedSchemaArnsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listAppliedSchemaArnsResponse_httpStatus' - The response's http status code.
newListAppliedSchemaArnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppliedSchemaArnsResponse
newListAppliedSchemaArnsResponse pHttpStatus_ =
  ListAppliedSchemaArnsResponse'
    { schemaArns =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs of schemas that are applied to the directory.
listAppliedSchemaArnsResponse_schemaArns :: Lens.Lens' ListAppliedSchemaArnsResponse (Prelude.Maybe [Prelude.Text])
listAppliedSchemaArnsResponse_schemaArns = Lens.lens (\ListAppliedSchemaArnsResponse' {schemaArns} -> schemaArns) (\s@ListAppliedSchemaArnsResponse' {} a -> s {schemaArns = a} :: ListAppliedSchemaArnsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token.
listAppliedSchemaArnsResponse_nextToken :: Lens.Lens' ListAppliedSchemaArnsResponse (Prelude.Maybe Prelude.Text)
listAppliedSchemaArnsResponse_nextToken = Lens.lens (\ListAppliedSchemaArnsResponse' {nextToken} -> nextToken) (\s@ListAppliedSchemaArnsResponse' {} a -> s {nextToken = a} :: ListAppliedSchemaArnsResponse)

-- | The response's http status code.
listAppliedSchemaArnsResponse_httpStatus :: Lens.Lens' ListAppliedSchemaArnsResponse Prelude.Int
listAppliedSchemaArnsResponse_httpStatus = Lens.lens (\ListAppliedSchemaArnsResponse' {httpStatus} -> httpStatus) (\s@ListAppliedSchemaArnsResponse' {} a -> s {httpStatus = a} :: ListAppliedSchemaArnsResponse)

instance Prelude.NFData ListAppliedSchemaArnsResponse
