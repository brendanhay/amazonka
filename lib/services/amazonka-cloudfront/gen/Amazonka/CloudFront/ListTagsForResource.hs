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
-- Module      : Amazonka.CloudFront.ListTagsForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List tags for a CloudFront resource.
module Amazonka.CloudFront.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resource,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to list tags for a CloudFront resource.
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'listTagsForResource_resource' - An ARN of a CloudFront resource.
newListTagsForResource ::
  -- | 'resource'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResource_ =
  ListTagsForResource' {resource = pResource_}

-- | An ARN of a CloudFront resource.
listTagsForResource_resource :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resource = Lens.lens (\ListTagsForResource' {resource} -> resource) (\s@ListTagsForResource' {} a -> s {resource = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.parseXML x)
      )

instance Prelude.Hashable ListTagsForResource where
  hashWithSalt _salt ListTagsForResource' {..} =
    _salt `Prelude.hashWithSalt` resource

instance Prelude.NFData ListTagsForResource where
  rnf ListTagsForResource' {..} = Prelude.rnf resource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTagsForResource where
  toPath = Prelude.const "/2020-05-31/tagging"

instance Core.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Prelude.mconcat ["Resource" Core.=: resource]

-- | The returned result of the corresponding request.
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listTagsForResourceResponse_tags' - A complex type that contains zero or more @Tag@ elements.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'tags'
  Tags ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ pTags_ =
  ListTagsForResourceResponse'
    { httpStatus =
        pHttpStatus_,
      tags = pTags_
    }

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

-- | A complex type that contains zero or more @Tag@ elements.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse Tags
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse where
  rnf ListTagsForResourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
