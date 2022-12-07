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
-- Module      : Amazonka.Glue.ListMLTransforms
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sortable, filterable list of existing Glue machine learning
-- transforms in this Amazon Web Services account, or the resources with
-- the specified tag. This operation takes the optional @Tags@ field, which
-- you can use as a filter of the responses so that tagged resources can be
-- retrieved as a group. If you choose to use tag filtering, only resources
-- with the tags are retrieved.
module Amazonka.Glue.ListMLTransforms
  ( -- * Creating a Request
    ListMLTransforms (..),
    newListMLTransforms,

    -- * Request Lenses
    listMLTransforms_tags,
    listMLTransforms_nextToken,
    listMLTransforms_sort,
    listMLTransforms_filter,
    listMLTransforms_maxResults,

    -- * Destructuring the Response
    ListMLTransformsResponse (..),
    newListMLTransformsResponse,

    -- * Response Lenses
    listMLTransformsResponse_nextToken,
    listMLTransformsResponse_httpStatus,
    listMLTransformsResponse_transformIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMLTransforms' smart constructor.
data ListMLTransforms = ListMLTransforms'
  { -- | Specifies to return only these tagged resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @TransformSortCriteria@ used to sort the machine learning transforms.
    sort :: Prelude.Maybe TransformSortCriteria,
    -- | A @TransformFilterCriteria@ used to filter the machine learning
    -- transforms.
    filter' :: Prelude.Maybe TransformFilterCriteria,
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMLTransforms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listMLTransforms_tags' - Specifies to return only these tagged resources.
--
-- 'nextToken', 'listMLTransforms_nextToken' - A continuation token, if this is a continuation request.
--
-- 'sort', 'listMLTransforms_sort' - A @TransformSortCriteria@ used to sort the machine learning transforms.
--
-- 'filter'', 'listMLTransforms_filter' - A @TransformFilterCriteria@ used to filter the machine learning
-- transforms.
--
-- 'maxResults', 'listMLTransforms_maxResults' - The maximum size of a list to return.
newListMLTransforms ::
  ListMLTransforms
newListMLTransforms =
  ListMLTransforms'
    { tags = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sort = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specifies to return only these tagged resources.
listMLTransforms_tags :: Lens.Lens' ListMLTransforms (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listMLTransforms_tags = Lens.lens (\ListMLTransforms' {tags} -> tags) (\s@ListMLTransforms' {} a -> s {tags = a} :: ListMLTransforms) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if this is a continuation request.
listMLTransforms_nextToken :: Lens.Lens' ListMLTransforms (Prelude.Maybe Prelude.Text)
listMLTransforms_nextToken = Lens.lens (\ListMLTransforms' {nextToken} -> nextToken) (\s@ListMLTransforms' {} a -> s {nextToken = a} :: ListMLTransforms)

-- | A @TransformSortCriteria@ used to sort the machine learning transforms.
listMLTransforms_sort :: Lens.Lens' ListMLTransforms (Prelude.Maybe TransformSortCriteria)
listMLTransforms_sort = Lens.lens (\ListMLTransforms' {sort} -> sort) (\s@ListMLTransforms' {} a -> s {sort = a} :: ListMLTransforms)

-- | A @TransformFilterCriteria@ used to filter the machine learning
-- transforms.
listMLTransforms_filter :: Lens.Lens' ListMLTransforms (Prelude.Maybe TransformFilterCriteria)
listMLTransforms_filter = Lens.lens (\ListMLTransforms' {filter'} -> filter') (\s@ListMLTransforms' {} a -> s {filter' = a} :: ListMLTransforms)

-- | The maximum size of a list to return.
listMLTransforms_maxResults :: Lens.Lens' ListMLTransforms (Prelude.Maybe Prelude.Natural)
listMLTransforms_maxResults = Lens.lens (\ListMLTransforms' {maxResults} -> maxResults) (\s@ListMLTransforms' {} a -> s {maxResults = a} :: ListMLTransforms)

instance Core.AWSRequest ListMLTransforms where
  type
    AWSResponse ListMLTransforms =
      ListMLTransformsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMLTransformsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "TransformIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListMLTransforms where
  hashWithSalt _salt ListMLTransforms' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMLTransforms where
  rnf ListMLTransforms' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListMLTransforms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ListMLTransforms" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMLTransforms where
  toJSON ListMLTransforms' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Sort" Data..=) Prelude.<$> sort,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListMLTransforms where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMLTransforms where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMLTransformsResponse' smart constructor.
data ListMLTransformsResponse = ListMLTransformsResponse'
  { -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifiers of all the machine learning transforms in the account,
    -- or the machine learning transforms with the specified tags.
    transformIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMLTransformsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMLTransformsResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'httpStatus', 'listMLTransformsResponse_httpStatus' - The response's http status code.
--
-- 'transformIds', 'listMLTransformsResponse_transformIds' - The identifiers of all the machine learning transforms in the account,
-- or the machine learning transforms with the specified tags.
newListMLTransformsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMLTransformsResponse
newListMLTransformsResponse pHttpStatus_ =
  ListMLTransformsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      transformIds = Prelude.mempty
    }

-- | A continuation token, if the returned list does not contain the last
-- metric available.
listMLTransformsResponse_nextToken :: Lens.Lens' ListMLTransformsResponse (Prelude.Maybe Prelude.Text)
listMLTransformsResponse_nextToken = Lens.lens (\ListMLTransformsResponse' {nextToken} -> nextToken) (\s@ListMLTransformsResponse' {} a -> s {nextToken = a} :: ListMLTransformsResponse)

-- | The response's http status code.
listMLTransformsResponse_httpStatus :: Lens.Lens' ListMLTransformsResponse Prelude.Int
listMLTransformsResponse_httpStatus = Lens.lens (\ListMLTransformsResponse' {httpStatus} -> httpStatus) (\s@ListMLTransformsResponse' {} a -> s {httpStatus = a} :: ListMLTransformsResponse)

-- | The identifiers of all the machine learning transforms in the account,
-- or the machine learning transforms with the specified tags.
listMLTransformsResponse_transformIds :: Lens.Lens' ListMLTransformsResponse [Prelude.Text]
listMLTransformsResponse_transformIds = Lens.lens (\ListMLTransformsResponse' {transformIds} -> transformIds) (\s@ListMLTransformsResponse' {} a -> s {transformIds = a} :: ListMLTransformsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListMLTransformsResponse where
  rnf ListMLTransformsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf transformIds
