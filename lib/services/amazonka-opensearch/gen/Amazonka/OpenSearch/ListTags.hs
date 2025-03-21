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
-- Module      : Amazonka.OpenSearch.ListTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all resource tags for an Amazon OpenSearch Service domain. For
-- more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-awsresourcetagging.html Tagging Amazon OpenSearch Service domains>.
module Amazonka.OpenSearch.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_arn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @ListTags@ operation.
--
-- /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | Amazon Resource Name (ARN) for the domain to view tags for.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listTags_arn' - Amazon Resource Name (ARN) for the domain to view tags for.
newListTags ::
  -- | 'arn'
  Prelude.Text ->
  ListTags
newListTags pARN_ = ListTags' {arn = pARN_}

-- | Amazon Resource Name (ARN) for the domain to view tags for.
listTags_arn :: Lens.Lens' ListTags Prelude.Text
listTags_arn = Lens.lens (\ListTags' {arn} -> arn) (\s@ListTags' {} a -> s {arn = a} :: ListTags)

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Prelude.<$> (x Data..?> "TagList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTags where
  hashWithSalt _salt ListTags' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData ListTags where
  rnf ListTags' {..} = Prelude.rnf arn

instance Data.ToHeaders ListTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTags where
  toPath = Prelude.const "/2021-01-01/tags/"

instance Data.ToQuery ListTags where
  toQuery ListTags' {..} =
    Prelude.mconcat ["arn" Data.=: arn]

-- | The results of a @ListTags@ operation.
--
-- /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | List of resource tags associated with the specified domain.
    tagList :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'listTagsResponse_tagList' - List of resource tags associated with the specified domain.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { tagList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of resource tags associated with the specified domain.
listTagsResponse_tagList :: Lens.Lens' ListTagsResponse (Prelude.Maybe [Tag])
listTagsResponse_tagList = Lens.lens (\ListTagsResponse' {tagList} -> tagList) (\s@ListTagsResponse' {} a -> s {tagList = a} :: ListTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Prelude.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Prelude.NFData ListTagsResponse where
  rnf ListTagsResponse' {..} =
    Prelude.rnf tagList `Prelude.seq`
      Prelude.rnf httpStatus
