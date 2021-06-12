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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMLTransforms' smart constructor.
data ListMLTransforms = ListMLTransforms'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of a list to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies to return only these tagged resources.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A @TransformFilterCriteria@ used to filter the machine learning
    -- transforms.
    filter' :: Core.Maybe TransformFilterCriteria,
    -- | A @TransformSortCriteria@ used to sort the machine learning transforms.
    sort :: Core.Maybe TransformSortCriteria
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      tags = Core.Nothing,
      filter' = Core.Nothing,
      sort = Core.Nothing
    }

-- | A continuation token, if this is a continuation request.
listMLTransforms_nextToken :: Lens.Lens' ListMLTransforms (Core.Maybe Core.Text)
listMLTransforms_nextToken = Lens.lens (\ListMLTransforms' {nextToken} -> nextToken) (\s@ListMLTransforms' {} a -> s {nextToken = a} :: ListMLTransforms)

-- | The maximum size of a list to return.
listMLTransforms_maxResults :: Lens.Lens' ListMLTransforms (Core.Maybe Core.Natural)
listMLTransforms_maxResults = Lens.lens (\ListMLTransforms' {maxResults} -> maxResults) (\s@ListMLTransforms' {} a -> s {maxResults = a} :: ListMLTransforms)

-- | Specifies to return only these tagged resources.
listMLTransforms_tags :: Lens.Lens' ListMLTransforms (Core.Maybe (Core.HashMap Core.Text Core.Text))
listMLTransforms_tags = Lens.lens (\ListMLTransforms' {tags} -> tags) (\s@ListMLTransforms' {} a -> s {tags = a} :: ListMLTransforms) Core.. Lens.mapping Lens._Coerce

-- | A @TransformFilterCriteria@ used to filter the machine learning
-- transforms.
listMLTransforms_filter :: Lens.Lens' ListMLTransforms (Core.Maybe TransformFilterCriteria)
listMLTransforms_filter = Lens.lens (\ListMLTransforms' {filter'} -> filter') (\s@ListMLTransforms' {} a -> s {filter' = a} :: ListMLTransforms)

-- | A @TransformSortCriteria@ used to sort the machine learning transforms.
listMLTransforms_sort :: Lens.Lens' ListMLTransforms (Core.Maybe TransformSortCriteria)
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "TransformIds" Core..!@ Core.mempty)
      )

instance Core.Hashable ListMLTransforms

instance Core.NFData ListMLTransforms

instance Core.ToHeaders ListMLTransforms where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListMLTransforms" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMLTransforms where
  toJSON ListMLTransforms' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Tags" Core..=) Core.<$> tags,
            ("Filter" Core..=) Core.<$> filter',
            ("Sort" Core..=) Core.<$> sort
          ]
      )

instance Core.ToPath ListMLTransforms where
  toPath = Core.const "/"

instance Core.ToQuery ListMLTransforms where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMLTransformsResponse' smart constructor.
data ListMLTransformsResponse = ListMLTransformsResponse'
  { -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The identifiers of all the machine learning transforms in the account,
    -- or the machine learning transforms with the specified tags.
    transformIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListMLTransformsResponse
newListMLTransformsResponse pHttpStatus_ =
  ListMLTransformsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      transformIds = Core.mempty
    }

-- | A continuation token, if the returned list does not contain the last
-- metric available.
listMLTransformsResponse_nextToken :: Lens.Lens' ListMLTransformsResponse (Core.Maybe Core.Text)
listMLTransformsResponse_nextToken = Lens.lens (\ListMLTransformsResponse' {nextToken} -> nextToken) (\s@ListMLTransformsResponse' {} a -> s {nextToken = a} :: ListMLTransformsResponse)

-- | The response's http status code.
listMLTransformsResponse_httpStatus :: Lens.Lens' ListMLTransformsResponse Core.Int
listMLTransformsResponse_httpStatus = Lens.lens (\ListMLTransformsResponse' {httpStatus} -> httpStatus) (\s@ListMLTransformsResponse' {} a -> s {httpStatus = a} :: ListMLTransformsResponse)

-- | The identifiers of all the machine learning transforms in the account,
-- or the machine learning transforms with the specified tags.
listMLTransformsResponse_transformIds :: Lens.Lens' ListMLTransformsResponse [Core.Text]
listMLTransformsResponse_transformIds = Lens.lens (\ListMLTransformsResponse' {transformIds} -> transformIds) (\s@ListMLTransformsResponse' {} a -> s {transformIds = a} :: ListMLTransformsResponse) Core.. Lens._Coerce

instance Core.NFData ListMLTransformsResponse
