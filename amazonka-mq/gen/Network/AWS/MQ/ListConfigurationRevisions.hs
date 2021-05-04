{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MQ.ListConfigurationRevisions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all revisions for the specified configuration.
module Network.AWS.MQ.ListConfigurationRevisions
  ( -- * Creating a Request
    ListConfigurationRevisions (..),
    newListConfigurationRevisions,

    -- * Request Lenses
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_configurationId,

    -- * Destructuring the Response
    ListConfigurationRevisionsResponse (..),
    newListConfigurationRevisionsResponse,

    -- * Response Lenses
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_maxResults,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_configurationId,
    listConfigurationRevisionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListConfigurationRevisions' smart constructor.
data ListConfigurationRevisions = ListConfigurationRevisions'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of configurations that Amazon MQ can return per page
    -- (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationRevisions_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'listConfigurationRevisions_maxResults' - The maximum number of configurations that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
--
-- 'configurationId', 'listConfigurationRevisions_configurationId' - The unique ID that Amazon MQ generates for the configuration.
newListConfigurationRevisions ::
  -- | 'configurationId'
  Prelude.Text ->
  ListConfigurationRevisions
newListConfigurationRevisions pConfigurationId_ =
  ListConfigurationRevisions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      configurationId = pConfigurationId_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listConfigurationRevisions_nextToken :: Lens.Lens' ListConfigurationRevisions (Prelude.Maybe Prelude.Text)
listConfigurationRevisions_nextToken = Lens.lens (\ListConfigurationRevisions' {nextToken} -> nextToken) (\s@ListConfigurationRevisions' {} a -> s {nextToken = a} :: ListConfigurationRevisions)

-- | The maximum number of configurations that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
listConfigurationRevisions_maxResults :: Lens.Lens' ListConfigurationRevisions (Prelude.Maybe Prelude.Natural)
listConfigurationRevisions_maxResults = Lens.lens (\ListConfigurationRevisions' {maxResults} -> maxResults) (\s@ListConfigurationRevisions' {} a -> s {maxResults = a} :: ListConfigurationRevisions)

-- | The unique ID that Amazon MQ generates for the configuration.
listConfigurationRevisions_configurationId :: Lens.Lens' ListConfigurationRevisions Prelude.Text
listConfigurationRevisions_configurationId = Lens.lens (\ListConfigurationRevisions' {configurationId} -> configurationId) (\s@ListConfigurationRevisions' {} a -> s {configurationId = a} :: ListConfigurationRevisions)

instance
  Prelude.AWSRequest
    ListConfigurationRevisions
  where
  type
    Rs ListConfigurationRevisions =
      ListConfigurationRevisionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationRevisionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "maxResults")
            Prelude.<*> ( x Prelude..?> "revisions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "configurationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationRevisions

instance Prelude.NFData ListConfigurationRevisions

instance Prelude.ToHeaders ListConfigurationRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListConfigurationRevisions where
  toPath ListConfigurationRevisions' {..} =
    Prelude.mconcat
      [ "/v1/configurations/",
        Prelude.toBS configurationId,
        "/revisions"
      ]

instance Prelude.ToQuery ListConfigurationRevisions where
  toQuery ListConfigurationRevisions' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListConfigurationRevisionsResponse' smart constructor.
data ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of configuration revisions that can be returned per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The list of all revisions for the specified configuration.
    revisions :: Prelude.Maybe [ConfigurationRevision],
    -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationRevisionsResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'listConfigurationRevisionsResponse_maxResults' - The maximum number of configuration revisions that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
--
-- 'revisions', 'listConfigurationRevisionsResponse_revisions' - The list of all revisions for the specified configuration.
--
-- 'configurationId', 'listConfigurationRevisionsResponse_configurationId' - The unique ID that Amazon MQ generates for the configuration.
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
      maxResults = Prelude.Nothing,
      revisions = Prelude.Nothing,
      configurationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listConfigurationRevisionsResponse_nextToken :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Text)
listConfigurationRevisionsResponse_nextToken = Lens.lens (\ListConfigurationRevisionsResponse' {nextToken} -> nextToken) (\s@ListConfigurationRevisionsResponse' {} a -> s {nextToken = a} :: ListConfigurationRevisionsResponse)

-- | The maximum number of configuration revisions that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
listConfigurationRevisionsResponse_maxResults :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Int)
listConfigurationRevisionsResponse_maxResults = Lens.lens (\ListConfigurationRevisionsResponse' {maxResults} -> maxResults) (\s@ListConfigurationRevisionsResponse' {} a -> s {maxResults = a} :: ListConfigurationRevisionsResponse)

-- | The list of all revisions for the specified configuration.
listConfigurationRevisionsResponse_revisions :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe [ConfigurationRevision])
listConfigurationRevisionsResponse_revisions = Lens.lens (\ListConfigurationRevisionsResponse' {revisions} -> revisions) (\s@ListConfigurationRevisionsResponse' {} a -> s {revisions = a} :: ListConfigurationRevisionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique ID that Amazon MQ generates for the configuration.
listConfigurationRevisionsResponse_configurationId :: Lens.Lens' ListConfigurationRevisionsResponse (Prelude.Maybe Prelude.Text)
listConfigurationRevisionsResponse_configurationId = Lens.lens (\ListConfigurationRevisionsResponse' {configurationId} -> configurationId) (\s@ListConfigurationRevisionsResponse' {} a -> s {configurationId = a} :: ListConfigurationRevisionsResponse)

-- | The response's http status code.
listConfigurationRevisionsResponse_httpStatus :: Lens.Lens' ListConfigurationRevisionsResponse Prelude.Int
listConfigurationRevisionsResponse_httpStatus = Lens.lens (\ListConfigurationRevisionsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationRevisionsResponse' {} a -> s {httpStatus = a} :: ListConfigurationRevisionsResponse)

instance
  Prelude.NFData
    ListConfigurationRevisionsResponse
