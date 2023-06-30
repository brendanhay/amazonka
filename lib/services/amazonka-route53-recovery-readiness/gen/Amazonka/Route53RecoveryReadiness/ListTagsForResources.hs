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
-- Module      : Amazonka.Route53RecoveryReadiness.ListTagsForResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for a resource.
module Amazonka.Route53RecoveryReadiness.ListTagsForResources
  ( -- * Creating a Request
    ListTagsForResources (..),
    newListTagsForResources,

    -- * Request Lenses
    listTagsForResources_resourceArn,

    -- * Destructuring the Response
    ListTagsForResourcesResponse (..),
    newListTagsForResourcesResponse,

    -- * Response Lenses
    listTagsForResourcesResponse_tags,
    listTagsForResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListTagsForResources' smart constructor.
data ListTagsForResources = ListTagsForResources'
  { -- | The Amazon Resource Name (ARN) for a resource.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTagsForResources_resourceArn' - The Amazon Resource Name (ARN) for a resource.
newListTagsForResources ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListTagsForResources
newListTagsForResources pResourceArn_ =
  ListTagsForResources' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) for a resource.
listTagsForResources_resourceArn :: Lens.Lens' ListTagsForResources Prelude.Text
listTagsForResources_resourceArn = Lens.lens (\ListTagsForResources' {resourceArn} -> resourceArn) (\s@ListTagsForResources' {} a -> s {resourceArn = a} :: ListTagsForResources)

instance Core.AWSRequest ListTagsForResources where
  type
    AWSResponse ListTagsForResources =
      ListTagsForResourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourcesResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResources where
  hashWithSalt _salt ListTagsForResources' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListTagsForResources where
  rnf ListTagsForResources' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders ListTagsForResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTagsForResources where
  toPath ListTagsForResources' {..} =
    Prelude.mconcat ["/tags/", Data.toBS resourceArn]

instance Data.ToQuery ListTagsForResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForResourcesResponse' smart constructor.
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsForResourcesResponse_tags' -
--
-- 'httpStatus', 'listTagsForResourcesResponse_httpStatus' - The response's http status code.
newListTagsForResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourcesResponse
newListTagsForResourcesResponse pHttpStatus_ =
  ListTagsForResourcesResponse'
    { tags =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

listTagsForResourcesResponse_tags :: Lens.Lens' ListTagsForResourcesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsForResourcesResponse_tags = Lens.lens (\ListTagsForResourcesResponse' {tags} -> tags) (\s@ListTagsForResourcesResponse' {} a -> s {tags = a} :: ListTagsForResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForResourcesResponse_httpStatus :: Lens.Lens' ListTagsForResourcesResponse Prelude.Int
listTagsForResourcesResponse_httpStatus = Lens.lens (\ListTagsForResourcesResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourcesResponse' {} a -> s {httpStatus = a} :: ListTagsForResourcesResponse)

instance Prelude.NFData ListTagsForResourcesResponse where
  rnf ListTagsForResourcesResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
