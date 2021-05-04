{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.ListFacetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes attached to the facet.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetAttributes
  ( -- * Creating a Request
    ListFacetAttributes (..),
    newListFacetAttributes,

    -- * Request Lenses
    listFacetAttributes_nextToken,
    listFacetAttributes_maxResults,
    listFacetAttributes_schemaArn,
    listFacetAttributes_name,

    -- * Destructuring the Response
    ListFacetAttributesResponse (..),
    newListFacetAttributesResponse,

    -- * Response Lenses
    listFacetAttributesResponse_nextToken,
    listFacetAttributesResponse_attributes,
    listFacetAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFacetAttributes' smart constructor.
data ListFacetAttributes = ListFacetAttributes'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the schema where the facet resides.
    schemaArn :: Prelude.Text,
    -- | The name of the facet whose attributes will be retrieved.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListFacetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFacetAttributes_nextToken' - The pagination token.
--
-- 'maxResults', 'listFacetAttributes_maxResults' - The maximum number of results to retrieve.
--
-- 'schemaArn', 'listFacetAttributes_schemaArn' - The ARN of the schema where the facet resides.
--
-- 'name', 'listFacetAttributes_name' - The name of the facet whose attributes will be retrieved.
newListFacetAttributes ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ListFacetAttributes
newListFacetAttributes pSchemaArn_ pName_ =
  ListFacetAttributes'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The pagination token.
listFacetAttributes_nextToken :: Lens.Lens' ListFacetAttributes (Prelude.Maybe Prelude.Text)
listFacetAttributes_nextToken = Lens.lens (\ListFacetAttributes' {nextToken} -> nextToken) (\s@ListFacetAttributes' {} a -> s {nextToken = a} :: ListFacetAttributes)

-- | The maximum number of results to retrieve.
listFacetAttributes_maxResults :: Lens.Lens' ListFacetAttributes (Prelude.Maybe Prelude.Natural)
listFacetAttributes_maxResults = Lens.lens (\ListFacetAttributes' {maxResults} -> maxResults) (\s@ListFacetAttributes' {} a -> s {maxResults = a} :: ListFacetAttributes)

-- | The ARN of the schema where the facet resides.
listFacetAttributes_schemaArn :: Lens.Lens' ListFacetAttributes Prelude.Text
listFacetAttributes_schemaArn = Lens.lens (\ListFacetAttributes' {schemaArn} -> schemaArn) (\s@ListFacetAttributes' {} a -> s {schemaArn = a} :: ListFacetAttributes)

-- | The name of the facet whose attributes will be retrieved.
listFacetAttributes_name :: Lens.Lens' ListFacetAttributes Prelude.Text
listFacetAttributes_name = Lens.lens (\ListFacetAttributes' {name} -> name) (\s@ListFacetAttributes' {} a -> s {name = a} :: ListFacetAttributes)

instance Pager.AWSPager ListFacetAttributes where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listFacetAttributesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listFacetAttributesResponse_attributes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listFacetAttributes_nextToken
          Lens..~ rs
          Lens.^? listFacetAttributesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListFacetAttributes where
  type
    Rs ListFacetAttributes =
      ListFacetAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetAttributesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "Attributes"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFacetAttributes

instance Prelude.NFData ListFacetAttributes

instance Prelude.ToHeaders ListFacetAttributes where
  toHeaders ListFacetAttributes' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# schemaArn]

instance Prelude.ToJSON ListFacetAttributes where
  toJSON ListFacetAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath ListFacetAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet/attributes"

instance Prelude.ToQuery ListFacetAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFacetAttributesResponse' smart constructor.
data ListFacetAttributesResponse = ListFacetAttributesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The attributes attached to the facet.
    attributes :: Prelude.Maybe [FacetAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListFacetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFacetAttributesResponse_nextToken' - The pagination token.
--
-- 'attributes', 'listFacetAttributesResponse_attributes' - The attributes attached to the facet.
--
-- 'httpStatus', 'listFacetAttributesResponse_httpStatus' - The response's http status code.
newListFacetAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFacetAttributesResponse
newListFacetAttributesResponse pHttpStatus_ =
  ListFacetAttributesResponse'
    { nextToken =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listFacetAttributesResponse_nextToken :: Lens.Lens' ListFacetAttributesResponse (Prelude.Maybe Prelude.Text)
listFacetAttributesResponse_nextToken = Lens.lens (\ListFacetAttributesResponse' {nextToken} -> nextToken) (\s@ListFacetAttributesResponse' {} a -> s {nextToken = a} :: ListFacetAttributesResponse)

-- | The attributes attached to the facet.
listFacetAttributesResponse_attributes :: Lens.Lens' ListFacetAttributesResponse (Prelude.Maybe [FacetAttribute])
listFacetAttributesResponse_attributes = Lens.lens (\ListFacetAttributesResponse' {attributes} -> attributes) (\s@ListFacetAttributesResponse' {} a -> s {attributes = a} :: ListFacetAttributesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listFacetAttributesResponse_httpStatus :: Lens.Lens' ListFacetAttributesResponse Prelude.Int
listFacetAttributesResponse_httpStatus = Lens.lens (\ListFacetAttributesResponse' {httpStatus} -> httpStatus) (\s@ListFacetAttributesResponse' {} a -> s {httpStatus = a} :: ListFacetAttributesResponse)

instance Prelude.NFData ListFacetAttributesResponse
