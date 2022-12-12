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
-- Module      : Amazonka.MQ.ListConfigurationRevisions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all revisions for the specified configuration.
module Amazonka.MQ.ListConfigurationRevisions
  ( -- * Creating a Request
    ListConfigurationRevisions (..),
    newListConfigurationRevisions,

    -- * Request Lenses
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_configurationId,

    -- * Destructuring the Response
    ListConfigurationRevisionsResponse (..),
    newListConfigurationRevisionsResponse,

    -- * Response Lenses
    listConfigurationRevisionsResponse_configurationId,
    listConfigurationRevisionsResponse_maxResults,
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConfigurationRevisions' smart constructor.
data ListConfigurationRevisions = ListConfigurationRevisions'
  { -- | The maximum number of brokers that Amazon MQ can return per page (20 by
    -- default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Prelude.Text
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
-- 'maxResults', 'listConfigurationRevisions_maxResults' - The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
--
-- 'nextToken', 'listConfigurationRevisions_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'configurationId', 'listConfigurationRevisions_configurationId' - The unique ID that Amazon MQ generates for the configuration.
newListConfigurationRevisions ::
  -- | 'configurationId'
  Prelude.Text ->
  ListConfigurationRevisions
newListConfigurationRevisions pConfigurationId_ =
  ListConfigurationRevisions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      configurationId = pConfigurationId_
    }

-- | The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
listConfigurationRevisions_maxResults :: Lens.Lens' ListConfigurationRevisions (Prelude.Maybe Prelude.Natural)
listConfigurationRevisions_maxResults = Lens.lens (\ListConfigurationRevisions' {maxResults} -> maxResults) (\s@ListConfigurationRevisions' {} a -> s {maxResults = a} :: ListConfigurationRevisions)

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listConfigurationRevisions_nextToken :: Lens.Lens' ListConfigurationRevisions (Prelude.Maybe Prelude.Text)
listConfigurationRevisions_nextToken = Lens.lens (\ListConfigurationRevisions' {nextToken} -> nextToken) (\s@ListConfigurationRevisions' {} a -> s {nextToken = a} :: ListConfigurationRevisions)

-- | The unique ID that Amazon MQ generates for the configuration.
listConfigurationRevisions_configurationId :: Lens.Lens' ListConfigurationRevisions Prelude.Text
listConfigurationRevisions_configurationId = Lens.lens (\ListConfigurationRevisions' {configurationId} -> configurationId) (\s@ListConfigurationRevisions' {} a -> s {configurationId = a} :: ListConfigurationRevisions)

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
            Prelude.<$> (x Data..?> "configurationId")
            Prelude.<*> (x Data..?> "maxResults")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "revisions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationRevisions where
  hashWithSalt _salt ListConfigurationRevisions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` configurationId

instance Prelude.NFData ListConfigurationRevisions where
  rnf ListConfigurationRevisions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf configurationId

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
      [ "/v1/configurations/",
        Data.toBS configurationId,
        "/revisions"
      ]

instance Data.ToQuery ListConfigurationRevisions where
  toQuery ListConfigurationRevisions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListConfigurationRevisionsResponse' smart constructor.
data ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse'
  { -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of configuration revisions that can be returned per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of all revisions for the specified configuration.
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
-- 'configurationId', 'listConfigurationRevisionsResponse_configurationId' - The unique ID that Amazon MQ generates for the configuration.
--
-- 'maxResults', 'listConfigurationRevisionsResponse_maxResults' - The maximum number of configuration revisions that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
--
-- 'nextToken', 'listConfigurationRevisionsResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'revisions', 'listConfigurationRevisionsResponse_revisions' - The list of all revisions for the specified configuration.
--
-- 'httpStatus', 'listConfigurationRevisionsResponse_httpStatus' - The response's http status code.
newListConfigurationRevisionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationRevisionsResponse
newListConfigurationRevisionsResponse pHttpStatus_ =
  ListConfigurationRevisionsResponse'
    { configurationId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      revisions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID that Amazon MQ generates for the configuration.
listConfigurationRevisionsResponse_configurationId :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Text)
listConfigurationRevisionsResponse_configurationId = Lens.lens (\ListConfigurationRevisionsResponse' {configurationId} -> configurationId) (\s@ListConfigurationRevisionsResponse' {} a -> s {configurationId = a} :: ListConfigurationRevisionsResponse)

-- | The maximum number of configuration revisions that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
listConfigurationRevisionsResponse_maxResults :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Int)
listConfigurationRevisionsResponse_maxResults = Lens.lens (\ListConfigurationRevisionsResponse' {maxResults} -> maxResults) (\s@ListConfigurationRevisionsResponse' {} a -> s {maxResults = a} :: ListConfigurationRevisionsResponse)

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listConfigurationRevisionsResponse_nextToken :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Text)
listConfigurationRevisionsResponse_nextToken = Lens.lens (\ListConfigurationRevisionsResponse' {nextToken} -> nextToken) (\s@ListConfigurationRevisionsResponse' {} a -> s {nextToken = a} :: ListConfigurationRevisionsResponse)

-- | The list of all revisions for the specified configuration.
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
    Prelude.rnf configurationId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf revisions
      `Prelude.seq` Prelude.rnf httpStatus
