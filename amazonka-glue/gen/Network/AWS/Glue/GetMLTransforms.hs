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
-- Module      : Network.AWS.Glue.GetMLTransforms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a sortable, filterable list of existing AWS Glue machine learning
-- transforms. Machine learning transforms are a special type of transform
-- that use machine learning to learn the details of the transformation to
-- be performed by learning from examples provided by humans. These
-- transformations are then saved by AWS Glue, and you can retrieve their
-- metadata by calling @GetMLTransforms@.
module Network.AWS.Glue.GetMLTransforms
  ( -- * Creating a Request
    GetMLTransforms (..),
    newGetMLTransforms,

    -- * Request Lenses
    getMLTransforms_nextToken,
    getMLTransforms_maxResults,
    getMLTransforms_filter,
    getMLTransforms_sort,

    -- * Destructuring the Response
    GetMLTransformsResponse (..),
    newGetMLTransformsResponse,

    -- * Response Lenses
    getMLTransformsResponse_nextToken,
    getMLTransformsResponse_httpStatus,
    getMLTransformsResponse_transforms,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMLTransforms' smart constructor.
data GetMLTransforms = GetMLTransforms'
  { -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filter transformation criteria.
    filter' :: Prelude.Maybe TransformFilterCriteria,
    -- | The sorting criteria.
    sort :: Prelude.Maybe TransformSortCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMLTransforms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMLTransforms_nextToken' - A paginated token to offset the results.
--
-- 'maxResults', 'getMLTransforms_maxResults' - The maximum number of results to return.
--
-- 'filter'', 'getMLTransforms_filter' - The filter transformation criteria.
--
-- 'sort', 'getMLTransforms_sort' - The sorting criteria.
newGetMLTransforms ::
  GetMLTransforms
newGetMLTransforms =
  GetMLTransforms'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing,
      sort = Prelude.Nothing
    }

-- | A paginated token to offset the results.
getMLTransforms_nextToken :: Lens.Lens' GetMLTransforms (Prelude.Maybe Prelude.Text)
getMLTransforms_nextToken = Lens.lens (\GetMLTransforms' {nextToken} -> nextToken) (\s@GetMLTransforms' {} a -> s {nextToken = a} :: GetMLTransforms)

-- | The maximum number of results to return.
getMLTransforms_maxResults :: Lens.Lens' GetMLTransforms (Prelude.Maybe Prelude.Natural)
getMLTransforms_maxResults = Lens.lens (\GetMLTransforms' {maxResults} -> maxResults) (\s@GetMLTransforms' {} a -> s {maxResults = a} :: GetMLTransforms)

-- | The filter transformation criteria.
getMLTransforms_filter :: Lens.Lens' GetMLTransforms (Prelude.Maybe TransformFilterCriteria)
getMLTransforms_filter = Lens.lens (\GetMLTransforms' {filter'} -> filter') (\s@GetMLTransforms' {} a -> s {filter' = a} :: GetMLTransforms)

-- | The sorting criteria.
getMLTransforms_sort :: Lens.Lens' GetMLTransforms (Prelude.Maybe TransformSortCriteria)
getMLTransforms_sort = Lens.lens (\GetMLTransforms' {sort} -> sort) (\s@GetMLTransforms' {} a -> s {sort = a} :: GetMLTransforms)

instance Core.AWSRequest GetMLTransforms where
  type
    AWSResponse GetMLTransforms =
      GetMLTransformsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTransformsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Transforms" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetMLTransforms

instance Prelude.NFData GetMLTransforms

instance Core.ToHeaders GetMLTransforms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetMLTransforms" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMLTransforms where
  toJSON GetMLTransforms' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filter" Core..=) Prelude.<$> filter',
            ("Sort" Core..=) Prelude.<$> sort
          ]
      )

instance Core.ToPath GetMLTransforms where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMLTransforms where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMLTransformsResponse' smart constructor.
data GetMLTransformsResponse = GetMLTransformsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of machine learning transforms.
    transforms :: [MLTransform]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMLTransformsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMLTransformsResponse_nextToken' - A pagination token, if more results are available.
--
-- 'httpStatus', 'getMLTransformsResponse_httpStatus' - The response's http status code.
--
-- 'transforms', 'getMLTransformsResponse_transforms' - A list of machine learning transforms.
newGetMLTransformsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMLTransformsResponse
newGetMLTransformsResponse pHttpStatus_ =
  GetMLTransformsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      transforms = Prelude.mempty
    }

-- | A pagination token, if more results are available.
getMLTransformsResponse_nextToken :: Lens.Lens' GetMLTransformsResponse (Prelude.Maybe Prelude.Text)
getMLTransformsResponse_nextToken = Lens.lens (\GetMLTransformsResponse' {nextToken} -> nextToken) (\s@GetMLTransformsResponse' {} a -> s {nextToken = a} :: GetMLTransformsResponse)

-- | The response's http status code.
getMLTransformsResponse_httpStatus :: Lens.Lens' GetMLTransformsResponse Prelude.Int
getMLTransformsResponse_httpStatus = Lens.lens (\GetMLTransformsResponse' {httpStatus} -> httpStatus) (\s@GetMLTransformsResponse' {} a -> s {httpStatus = a} :: GetMLTransformsResponse)

-- | A list of machine learning transforms.
getMLTransformsResponse_transforms :: Lens.Lens' GetMLTransformsResponse [MLTransform]
getMLTransformsResponse_transforms = Lens.lens (\GetMLTransformsResponse' {transforms} -> transforms) (\s@GetMLTransformsResponse' {} a -> s {transforms = a} :: GetMLTransformsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData GetMLTransformsResponse
