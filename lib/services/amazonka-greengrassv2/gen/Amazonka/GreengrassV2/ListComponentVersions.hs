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
-- Module      : Amazonka.GreengrassV2.ListComponentVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of all versions for a component. Greater
-- versions are listed first.
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListComponentVersions
  ( -- * Creating a Request
    ListComponentVersions (..),
    newListComponentVersions,

    -- * Request Lenses
    listComponentVersions_nextToken,
    listComponentVersions_maxResults,
    listComponentVersions_arn,

    -- * Destructuring the Response
    ListComponentVersionsResponse (..),
    newListComponentVersionsResponse,

    -- * Response Lenses
    listComponentVersionsResponse_nextToken,
    listComponentVersionsResponse_componentVersions,
    listComponentVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponentVersions' smart constructor.
data ListComponentVersions = ListComponentVersions'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentVersions_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'listComponentVersions_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'arn', 'listComponentVersions_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component.
newListComponentVersions ::
  -- | 'arn'
  Prelude.Text ->
  ListComponentVersions
newListComponentVersions pArn_ =
  ListComponentVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      arn = pArn_
    }

-- | The token to be used for the next set of paginated results.
listComponentVersions_nextToken :: Lens.Lens' ListComponentVersions (Prelude.Maybe Prelude.Text)
listComponentVersions_nextToken = Lens.lens (\ListComponentVersions' {nextToken} -> nextToken) (\s@ListComponentVersions' {} a -> s {nextToken = a} :: ListComponentVersions)

-- | The maximum number of results to be returned per paginated request.
listComponentVersions_maxResults :: Lens.Lens' ListComponentVersions (Prelude.Maybe Prelude.Natural)
listComponentVersions_maxResults = Lens.lens (\ListComponentVersions' {maxResults} -> maxResults) (\s@ListComponentVersions' {} a -> s {maxResults = a} :: ListComponentVersions)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component.
listComponentVersions_arn :: Lens.Lens' ListComponentVersions Prelude.Text
listComponentVersions_arn = Lens.lens (\ListComponentVersions' {arn} -> arn) (\s@ListComponentVersions' {} a -> s {arn = a} :: ListComponentVersions)

instance Core.AWSPager ListComponentVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComponentVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listComponentVersionsResponse_componentVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listComponentVersions_nextToken
          Lens..~ rs
          Lens.^? listComponentVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListComponentVersions where
  type
    AWSResponse ListComponentVersions =
      ListComponentVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentVersionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "componentVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComponentVersions where
  hashWithSalt _salt ListComponentVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListComponentVersions where
  rnf ListComponentVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf arn

instance Core.ToHeaders ListComponentVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListComponentVersions where
  toPath ListComponentVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/components/",
        Core.toBS arn,
        "/versions"
      ]

instance Core.ToQuery ListComponentVersions where
  toQuery ListComponentVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListComponentVersionsResponse' smart constructor.
data ListComponentVersionsResponse = ListComponentVersionsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of versions that exist for the component.
    componentVersions :: Prelude.Maybe [ComponentVersionListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentVersionsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'componentVersions', 'listComponentVersionsResponse_componentVersions' - A list of versions that exist for the component.
--
-- 'httpStatus', 'listComponentVersionsResponse_httpStatus' - The response's http status code.
newListComponentVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentVersionsResponse
newListComponentVersionsResponse pHttpStatus_ =
  ListComponentVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      componentVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listComponentVersionsResponse_nextToken :: Lens.Lens' ListComponentVersionsResponse (Prelude.Maybe Prelude.Text)
listComponentVersionsResponse_nextToken = Lens.lens (\ListComponentVersionsResponse' {nextToken} -> nextToken) (\s@ListComponentVersionsResponse' {} a -> s {nextToken = a} :: ListComponentVersionsResponse)

-- | A list of versions that exist for the component.
listComponentVersionsResponse_componentVersions :: Lens.Lens' ListComponentVersionsResponse (Prelude.Maybe [ComponentVersionListItem])
listComponentVersionsResponse_componentVersions = Lens.lens (\ListComponentVersionsResponse' {componentVersions} -> componentVersions) (\s@ListComponentVersionsResponse' {} a -> s {componentVersions = a} :: ListComponentVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listComponentVersionsResponse_httpStatus :: Lens.Lens' ListComponentVersionsResponse Prelude.Int
listComponentVersionsResponse_httpStatus = Lens.lens (\ListComponentVersionsResponse' {httpStatus} -> httpStatus) (\s@ListComponentVersionsResponse' {} a -> s {httpStatus = a} :: ListComponentVersionsResponse)

instance Prelude.NFData ListComponentVersionsResponse where
  rnf ListComponentVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf componentVersions
      `Prelude.seq` Prelude.rnf httpStatus
