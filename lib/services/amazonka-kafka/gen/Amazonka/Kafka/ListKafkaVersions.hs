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
-- Module      : Amazonka.Kafka.ListKafkaVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Apache Kafka versions.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListKafkaVersions
  ( -- * Creating a Request
    ListKafkaVersions (..),
    newListKafkaVersions,

    -- * Request Lenses
    listKafkaVersions_nextToken,
    listKafkaVersions_maxResults,

    -- * Destructuring the Response
    ListKafkaVersionsResponse (..),
    newListKafkaVersionsResponse,

    -- * Response Lenses
    listKafkaVersionsResponse_nextToken,
    listKafkaVersionsResponse_kafkaVersions,
    listKafkaVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKafkaVersions' smart constructor.
data ListKafkaVersions = ListKafkaVersions'
  { -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKafkaVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKafkaVersions_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
--
-- 'maxResults', 'listKafkaVersions_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
newListKafkaVersions ::
  ListKafkaVersions
newListKafkaVersions =
  ListKafkaVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listKafkaVersions_nextToken :: Lens.Lens' ListKafkaVersions (Prelude.Maybe Prelude.Text)
listKafkaVersions_nextToken = Lens.lens (\ListKafkaVersions' {nextToken} -> nextToken) (\s@ListKafkaVersions' {} a -> s {nextToken = a} :: ListKafkaVersions)

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listKafkaVersions_maxResults :: Lens.Lens' ListKafkaVersions (Prelude.Maybe Prelude.Natural)
listKafkaVersions_maxResults = Lens.lens (\ListKafkaVersions' {maxResults} -> maxResults) (\s@ListKafkaVersions' {} a -> s {maxResults = a} :: ListKafkaVersions)

instance Core.AWSPager ListKafkaVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKafkaVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listKafkaVersionsResponse_kafkaVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listKafkaVersions_nextToken
          Lens..~ rs
          Lens.^? listKafkaVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListKafkaVersions where
  type
    AWSResponse ListKafkaVersions =
      ListKafkaVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKafkaVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "kafkaVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKafkaVersions where
  hashWithSalt _salt ListKafkaVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListKafkaVersions where
  rnf ListKafkaVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListKafkaVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKafkaVersions where
  toPath = Prelude.const "/v1/kafka-versions"

instance Data.ToQuery ListKafkaVersions where
  toQuery ListKafkaVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListKafkaVersionsResponse' smart constructor.
data ListKafkaVersionsResponse = ListKafkaVersionsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    kafkaVersions :: Prelude.Maybe [KafkaVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKafkaVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKafkaVersionsResponse_nextToken' - Undocumented member.
--
-- 'kafkaVersions', 'listKafkaVersionsResponse_kafkaVersions' - Undocumented member.
--
-- 'httpStatus', 'listKafkaVersionsResponse_httpStatus' - The response's http status code.
newListKafkaVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKafkaVersionsResponse
newListKafkaVersionsResponse pHttpStatus_ =
  ListKafkaVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      kafkaVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listKafkaVersionsResponse_nextToken :: Lens.Lens' ListKafkaVersionsResponse (Prelude.Maybe Prelude.Text)
listKafkaVersionsResponse_nextToken = Lens.lens (\ListKafkaVersionsResponse' {nextToken} -> nextToken) (\s@ListKafkaVersionsResponse' {} a -> s {nextToken = a} :: ListKafkaVersionsResponse)

-- | Undocumented member.
listKafkaVersionsResponse_kafkaVersions :: Lens.Lens' ListKafkaVersionsResponse (Prelude.Maybe [KafkaVersion])
listKafkaVersionsResponse_kafkaVersions = Lens.lens (\ListKafkaVersionsResponse' {kafkaVersions} -> kafkaVersions) (\s@ListKafkaVersionsResponse' {} a -> s {kafkaVersions = a} :: ListKafkaVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listKafkaVersionsResponse_httpStatus :: Lens.Lens' ListKafkaVersionsResponse Prelude.Int
listKafkaVersionsResponse_httpStatus = Lens.lens (\ListKafkaVersionsResponse' {httpStatus} -> httpStatus) (\s@ListKafkaVersionsResponse' {} a -> s {httpStatus = a} :: ListKafkaVersionsResponse)

instance Prelude.NFData ListKafkaVersionsResponse where
  rnf ListKafkaVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf kafkaVersions
      `Prelude.seq` Prelude.rnf httpStatus
