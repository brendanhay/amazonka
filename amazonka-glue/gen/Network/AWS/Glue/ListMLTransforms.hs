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
-- Module      : Network.AWS.Glue.ListMLTransforms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sortable, filterable list of existing AWS Glue machine
-- learning transforms in this AWS account, or the resources with the
-- specified tag. This operation takes the optional @Tags@ field, which you
-- can use as a filter of the responses so that tagged resources can be
-- retrieved as a group. If you choose to use tag filtering, only resources
-- with the tags are retrieved.
module Network.AWS.Glue.ListMLTransforms
  ( -- * Creating a Request
    ListMLTransforms (..),
    newListMLTransforms,

    -- * Request Lenses
    listMLTransforms_nextToken,
    listMLTransforms_maxResults,
    listMLTransforms_tags,
    listMLTransforms_filter,
    listMLTransforms_sort,

    -- * Destructuring the Response
    ListMLTransformsResponse (..),
    newListMLTransformsResponse,

    -- * Response Lenses
    listMLTransformsResponse_nextToken,
    listMLTransformsResponse_httpStatus,
    listMLTransformsResponse_transformIds,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMLTransforms' smart constructor.
data ListMLTransforms = ListMLTransforms'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies to return only these tagged resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A @TransformFilterCriteria@ used to filter the machine learning
    -- transforms.
    filter' :: Prelude.Maybe TransformFilterCriteria,
    -- | A @TransformSortCriteria@ used to sort the machine learning transforms.
    sort :: Prelude.Maybe TransformSortCriteria
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
-- 'nextToken', 'listMLTransforms_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'listMLTransforms_maxResults' - The maximum size of a list to return.
--
-- 'tags', 'listMLTransforms_tags' - Specifies to return only these tagged resources.
--
-- 'filter'', 'listMLTransforms_filter' - A @TransformFilterCriteria@ used to filter the machine learning
-- transforms.
--
-- 'sort', 'listMLTransforms_sort' - A @TransformSortCriteria@ used to sort the machine learning transforms.
newListMLTransforms ::
  ListMLTransforms
newListMLTransforms =
  ListMLTransforms'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      tags = Prelude.Nothing,
      filter' = Prelude.Nothing,
      sort = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation request.
listMLTransforms_nextToken :: Lens.Lens' ListMLTransforms (Prelude.Maybe Prelude.Text)
listMLTransforms_nextToken = Lens.lens (\ListMLTransforms' {nextToken} -> nextToken) (\s@ListMLTransforms' {} a -> s {nextToken = a} :: ListMLTransforms)

-- | The maximum size of a list to return.
listMLTransforms_maxResults :: Lens.Lens' ListMLTransforms (Prelude.Maybe Prelude.Natural)
listMLTransforms_maxResults = Lens.lens (\ListMLTransforms' {maxResults} -> maxResults) (\s@ListMLTransforms' {} a -> s {maxResults = a} :: ListMLTransforms)

-- | Specifies to return only these tagged resources.
listMLTransforms_tags :: Lens.Lens' ListMLTransforms (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listMLTransforms_tags = Lens.lens (\ListMLTransforms' {tags} -> tags) (\s@ListMLTransforms' {} a -> s {tags = a} :: ListMLTransforms) Prelude.. Lens.mapping Lens._Coerce

-- | A @TransformFilterCriteria@ used to filter the machine learning
-- transforms.
listMLTransforms_filter :: Lens.Lens' ListMLTransforms (Prelude.Maybe TransformFilterCriteria)
listMLTransforms_filter = Lens.lens (\ListMLTransforms' {filter'} -> filter') (\s@ListMLTransforms' {} a -> s {filter' = a} :: ListMLTransforms)

-- | A @TransformSortCriteria@ used to sort the machine learning transforms.
listMLTransforms_sort :: Lens.Lens' ListMLTransforms (Prelude.Maybe TransformSortCriteria)
listMLTransforms_sort = Lens.lens (\ListMLTransforms' {sort} -> sort) (\s@ListMLTransforms' {} a -> s {sort = a} :: ListMLTransforms)

instance Core.AWSRequest ListMLTransforms where
  type
    AWSResponse ListMLTransforms =
      ListMLTransformsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMLTransformsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "TransformIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListMLTransforms

instance Prelude.NFData ListMLTransforms

instance Core.ToHeaders ListMLTransforms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListMLTransforms" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMLTransforms where
  toJSON ListMLTransforms' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Filter" Core..=) Prelude.<$> filter',
            ("Sort" Core..=) Prelude.<$> sort
          ]
      )

instance Core.ToPath ListMLTransforms where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMLTransforms where
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
listMLTransformsResponse_transformIds = Lens.lens (\ListMLTransformsResponse' {transformIds} -> transformIds) (\s@ListMLTransformsResponse' {} a -> s {transformIds = a} :: ListMLTransformsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListMLTransformsResponse
