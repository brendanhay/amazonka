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
-- Module      : Amazonka.CloudDirectory.ListTypedLinkFacetAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute definitions for a particular
-- TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListTypedLinkFacetAttributes
  ( -- * Creating a Request
    ListTypedLinkFacetAttributes (..),
    newListTypedLinkFacetAttributes,

    -- * Request Lenses
    listTypedLinkFacetAttributes_maxResults,
    listTypedLinkFacetAttributes_nextToken,
    listTypedLinkFacetAttributes_schemaArn,
    listTypedLinkFacetAttributes_name,

    -- * Destructuring the Response
    ListTypedLinkFacetAttributesResponse (..),
    newListTypedLinkFacetAttributesResponse,

    -- * Response Lenses
    listTypedLinkFacetAttributesResponse_attributes,
    listTypedLinkFacetAttributesResponse_nextToken,
    listTypedLinkFacetAttributesResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTypedLinkFacetAttributes' smart constructor.
data ListTypedLinkFacetAttributes = ListTypedLinkFacetAttributes'
  { -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The unique name of the typed link facet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypedLinkFacetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTypedLinkFacetAttributes_maxResults' - The maximum number of results to retrieve.
--
-- 'nextToken', 'listTypedLinkFacetAttributes_nextToken' - The pagination token.
--
-- 'schemaArn', 'listTypedLinkFacetAttributes_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
--
-- 'name', 'listTypedLinkFacetAttributes_name' - The unique name of the typed link facet.
newListTypedLinkFacetAttributes ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ListTypedLinkFacetAttributes
newListTypedLinkFacetAttributes pSchemaArn_ pName_ =
  ListTypedLinkFacetAttributes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The maximum number of results to retrieve.
listTypedLinkFacetAttributes_maxResults :: Lens.Lens' ListTypedLinkFacetAttributes (Prelude.Maybe Prelude.Natural)
listTypedLinkFacetAttributes_maxResults = Lens.lens (\ListTypedLinkFacetAttributes' {maxResults} -> maxResults) (\s@ListTypedLinkFacetAttributes' {} a -> s {maxResults = a} :: ListTypedLinkFacetAttributes)

-- | The pagination token.
listTypedLinkFacetAttributes_nextToken :: Lens.Lens' ListTypedLinkFacetAttributes (Prelude.Maybe Prelude.Text)
listTypedLinkFacetAttributes_nextToken = Lens.lens (\ListTypedLinkFacetAttributes' {nextToken} -> nextToken) (\s@ListTypedLinkFacetAttributes' {} a -> s {nextToken = a} :: ListTypedLinkFacetAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
listTypedLinkFacetAttributes_schemaArn :: Lens.Lens' ListTypedLinkFacetAttributes Prelude.Text
listTypedLinkFacetAttributes_schemaArn = Lens.lens (\ListTypedLinkFacetAttributes' {schemaArn} -> schemaArn) (\s@ListTypedLinkFacetAttributes' {} a -> s {schemaArn = a} :: ListTypedLinkFacetAttributes)

-- | The unique name of the typed link facet.
listTypedLinkFacetAttributes_name :: Lens.Lens' ListTypedLinkFacetAttributes Prelude.Text
listTypedLinkFacetAttributes_name = Lens.lens (\ListTypedLinkFacetAttributes' {name} -> name) (\s@ListTypedLinkFacetAttributes' {} a -> s {name = a} :: ListTypedLinkFacetAttributes)

instance Core.AWSPager ListTypedLinkFacetAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTypedLinkFacetAttributesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTypedLinkFacetAttributesResponse_attributes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTypedLinkFacetAttributes_nextToken
          Lens..~ rs
          Lens.^? listTypedLinkFacetAttributesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTypedLinkFacetAttributes where
  type
    AWSResponse ListTypedLinkFacetAttributes =
      ListTypedLinkFacetAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTypedLinkFacetAttributesResponse'
            Prelude.<$> (x Data..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTypedLinkFacetAttributes
  where
  hashWithSalt _salt ListTypedLinkFacetAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListTypedLinkFacetAttributes where
  rnf ListTypedLinkFacetAttributes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ListTypedLinkFacetAttributes where
  toHeaders ListTypedLinkFacetAttributes' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON ListTypedLinkFacetAttributes where
  toJSON ListTypedLinkFacetAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath ListTypedLinkFacetAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/attributes"

instance Data.ToQuery ListTypedLinkFacetAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTypedLinkFacetAttributesResponse' smart constructor.
data ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse'
  { -- | An ordered set of attributes associate with the typed link.
    attributes :: Prelude.Maybe [TypedLinkAttributeDefinition],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypedLinkFacetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'listTypedLinkFacetAttributesResponse_attributes' - An ordered set of attributes associate with the typed link.
--
-- 'nextToken', 'listTypedLinkFacetAttributesResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listTypedLinkFacetAttributesResponse_httpStatus' - The response's http status code.
newListTypedLinkFacetAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypedLinkFacetAttributesResponse
newListTypedLinkFacetAttributesResponse pHttpStatus_ =
  ListTypedLinkFacetAttributesResponse'
    { attributes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ordered set of attributes associate with the typed link.
listTypedLinkFacetAttributesResponse_attributes :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Prelude.Maybe [TypedLinkAttributeDefinition])
listTypedLinkFacetAttributesResponse_attributes = Lens.lens (\ListTypedLinkFacetAttributesResponse' {attributes} -> attributes) (\s@ListTypedLinkFacetAttributesResponse' {} a -> s {attributes = a} :: ListTypedLinkFacetAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
listTypedLinkFacetAttributesResponse_nextToken :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Prelude.Maybe Prelude.Text)
listTypedLinkFacetAttributesResponse_nextToken = Lens.lens (\ListTypedLinkFacetAttributesResponse' {nextToken} -> nextToken) (\s@ListTypedLinkFacetAttributesResponse' {} a -> s {nextToken = a} :: ListTypedLinkFacetAttributesResponse)

-- | The response's http status code.
listTypedLinkFacetAttributesResponse_httpStatus :: Lens.Lens' ListTypedLinkFacetAttributesResponse Prelude.Int
listTypedLinkFacetAttributesResponse_httpStatus = Lens.lens (\ListTypedLinkFacetAttributesResponse' {httpStatus} -> httpStatus) (\s@ListTypedLinkFacetAttributesResponse' {} a -> s {httpStatus = a} :: ListTypedLinkFacetAttributesResponse)

instance
  Prelude.NFData
    ListTypedLinkFacetAttributesResponse
  where
  rnf ListTypedLinkFacetAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
