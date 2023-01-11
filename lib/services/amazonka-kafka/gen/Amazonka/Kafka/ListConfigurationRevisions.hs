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
-- Module      : Amazonka.Kafka.ListConfigurationRevisions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the MSK configurations in this Region.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListConfigurationRevisions
  ( -- * Creating a Request
    ListConfigurationRevisions (..),
    newListConfigurationRevisions,

    -- * Request Lenses
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_arn,

    -- * Destructuring the Response
    ListConfigurationRevisionsResponse (..),
    newListConfigurationRevisionsResponse,

    -- * Response Lenses
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConfigurationRevisions' smart constructor.
data ListConfigurationRevisions = ListConfigurationRevisions'
  { -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
    -- configuration and all of its revisions.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConfigurationRevisions_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'nextToken', 'listConfigurationRevisions_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
--
-- 'arn', 'listConfigurationRevisions_arn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration and all of its revisions.
newListConfigurationRevisions ::
  -- | 'arn'
  Prelude.Text ->
  ListConfigurationRevisions
newListConfigurationRevisions pArn_ =
  ListConfigurationRevisions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      arn = pArn_
    }

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listConfigurationRevisions_maxResults :: Lens.Lens' ListConfigurationRevisions (Prelude.Maybe Prelude.Natural)
listConfigurationRevisions_maxResults = Lens.lens (\ListConfigurationRevisions' {maxResults} -> maxResults) (\s@ListConfigurationRevisions' {} a -> s {maxResults = a} :: ListConfigurationRevisions)

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listConfigurationRevisions_nextToken :: Lens.Lens' ListConfigurationRevisions (Prelude.Maybe Prelude.Text)
listConfigurationRevisions_nextToken = Lens.lens (\ListConfigurationRevisions' {nextToken} -> nextToken) (\s@ListConfigurationRevisions' {} a -> s {nextToken = a} :: ListConfigurationRevisions)

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration and all of its revisions.
listConfigurationRevisions_arn :: Lens.Lens' ListConfigurationRevisions Prelude.Text
listConfigurationRevisions_arn = Lens.lens (\ListConfigurationRevisions' {arn} -> arn) (\s@ListConfigurationRevisions' {} a -> s {arn = a} :: ListConfigurationRevisions)

instance Core.AWSPager ListConfigurationRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigurationRevisionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigurationRevisionsResponse_revisions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConfigurationRevisions_nextToken
          Lens..~ rs
          Lens.^? listConfigurationRevisionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListConfigurationRevisions where
  type
    AWSResponse ListConfigurationRevisions =
      ListConfigurationRevisionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationRevisionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "revisions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationRevisions where
  hashWithSalt _salt ListConfigurationRevisions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListConfigurationRevisions where
  rnf ListConfigurationRevisions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders ListConfigurationRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConfigurationRevisions where
  toPath ListConfigurationRevisions' {..} =
    Prelude.mconcat
      ["/v1/configurations/", Data.toBS arn, "/revisions"]

instance Data.ToQuery ListConfigurationRevisions where
  toQuery ListConfigurationRevisions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListConfigurationRevisionsResponse' smart constructor.
data ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse'
  { -- | Paginated results marker.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of ConfigurationRevision objects.
    revisions :: Prelude.Maybe [ConfigurationRevision],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationRevisionsResponse_nextToken' - Paginated results marker.
--
-- 'revisions', 'listConfigurationRevisionsResponse_revisions' - List of ConfigurationRevision objects.
--
-- 'httpStatus', 'listConfigurationRevisionsResponse_httpStatus' - The response's http status code.
newListConfigurationRevisionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationRevisionsResponse
newListConfigurationRevisionsResponse pHttpStatus_ =
  ListConfigurationRevisionsResponse'
    { nextToken =
        Prelude.Nothing,
      revisions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Paginated results marker.
listConfigurationRevisionsResponse_nextToken :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Text)
listConfigurationRevisionsResponse_nextToken = Lens.lens (\ListConfigurationRevisionsResponse' {nextToken} -> nextToken) (\s@ListConfigurationRevisionsResponse' {} a -> s {nextToken = a} :: ListConfigurationRevisionsResponse)

-- | List of ConfigurationRevision objects.
listConfigurationRevisionsResponse_revisions :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe [ConfigurationRevision])
listConfigurationRevisionsResponse_revisions = Lens.lens (\ListConfigurationRevisionsResponse' {revisions} -> revisions) (\s@ListConfigurationRevisionsResponse' {} a -> s {revisions = a} :: ListConfigurationRevisionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listConfigurationRevisionsResponse_httpStatus :: Lens.Lens' ListConfigurationRevisionsResponse Prelude.Int
listConfigurationRevisionsResponse_httpStatus = Lens.lens (\ListConfigurationRevisionsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationRevisionsResponse' {} a -> s {httpStatus = a} :: ListConfigurationRevisionsResponse)

instance
  Prelude.NFData
    ListConfigurationRevisionsResponse
  where
  rnf ListConfigurationRevisionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf revisions
      `Prelude.seq` Prelude.rnf httpStatus
