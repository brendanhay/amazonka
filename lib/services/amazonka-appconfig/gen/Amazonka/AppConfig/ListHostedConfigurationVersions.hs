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
-- Module      : Amazonka.AppConfig.ListHostedConfigurationVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists configurations stored in the AppConfig hosted configuration store
-- by version.
module Amazonka.AppConfig.ListHostedConfigurationVersions
  ( -- * Creating a Request
    ListHostedConfigurationVersions (..),
    newListHostedConfigurationVersions,

    -- * Request Lenses
    listHostedConfigurationVersions_nextToken,
    listHostedConfigurationVersions_maxResults,
    listHostedConfigurationVersions_applicationId,
    listHostedConfigurationVersions_configurationProfileId,

    -- * Destructuring the Response
    ListHostedConfigurationVersionsResponse (..),
    newListHostedConfigurationVersionsResponse,

    -- * Response Lenses
    listHostedConfigurationVersionsResponse_items,
    listHostedConfigurationVersionsResponse_nextToken,
    listHostedConfigurationVersionsResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHostedConfigurationVersions' smart constructor.
data ListHostedConfigurationVersions = ListHostedConfigurationVersions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The configuration profile ID.
    configurationProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostedConfigurationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHostedConfigurationVersions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listHostedConfigurationVersions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'applicationId', 'listHostedConfigurationVersions_applicationId' - The application ID.
--
-- 'configurationProfileId', 'listHostedConfigurationVersions_configurationProfileId' - The configuration profile ID.
newListHostedConfigurationVersions ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  ListHostedConfigurationVersions
newListHostedConfigurationVersions
  pApplicationId_
  pConfigurationProfileId_ =
    ListHostedConfigurationVersions'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        applicationId = pApplicationId_,
        configurationProfileId =
          pConfigurationProfileId_
      }

-- | A token to start the list. Use this token to get the next set of
-- results.
listHostedConfigurationVersions_nextToken :: Lens.Lens' ListHostedConfigurationVersions (Prelude.Maybe Prelude.Text)
listHostedConfigurationVersions_nextToken = Lens.lens (\ListHostedConfigurationVersions' {nextToken} -> nextToken) (\s@ListHostedConfigurationVersions' {} a -> s {nextToken = a} :: ListHostedConfigurationVersions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listHostedConfigurationVersions_maxResults :: Lens.Lens' ListHostedConfigurationVersions (Prelude.Maybe Prelude.Natural)
listHostedConfigurationVersions_maxResults = Lens.lens (\ListHostedConfigurationVersions' {maxResults} -> maxResults) (\s@ListHostedConfigurationVersions' {} a -> s {maxResults = a} :: ListHostedConfigurationVersions)

-- | The application ID.
listHostedConfigurationVersions_applicationId :: Lens.Lens' ListHostedConfigurationVersions Prelude.Text
listHostedConfigurationVersions_applicationId = Lens.lens (\ListHostedConfigurationVersions' {applicationId} -> applicationId) (\s@ListHostedConfigurationVersions' {} a -> s {applicationId = a} :: ListHostedConfigurationVersions)

-- | The configuration profile ID.
listHostedConfigurationVersions_configurationProfileId :: Lens.Lens' ListHostedConfigurationVersions Prelude.Text
listHostedConfigurationVersions_configurationProfileId = Lens.lens (\ListHostedConfigurationVersions' {configurationProfileId} -> configurationProfileId) (\s@ListHostedConfigurationVersions' {} a -> s {configurationProfileId = a} :: ListHostedConfigurationVersions)

instance
  Core.AWSRequest
    ListHostedConfigurationVersions
  where
  type
    AWSResponse ListHostedConfigurationVersions =
      ListHostedConfigurationVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHostedConfigurationVersionsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListHostedConfigurationVersions
  where
  hashWithSalt
    _salt
    ListHostedConfigurationVersions' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` applicationId
        `Prelude.hashWithSalt` configurationProfileId

instance
  Prelude.NFData
    ListHostedConfigurationVersions
  where
  rnf ListHostedConfigurationVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId

instance
  Data.ToHeaders
    ListHostedConfigurationVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListHostedConfigurationVersions where
  toPath ListHostedConfigurationVersions' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/configurationprofiles/",
        Data.toBS configurationProfileId,
        "/hostedconfigurationversions"
      ]

instance Data.ToQuery ListHostedConfigurationVersions where
  toQuery ListHostedConfigurationVersions' {..} =
    Prelude.mconcat
      [ "next_token" Data.=: nextToken,
        "max_results" Data.=: maxResults
      ]

-- | /See:/ 'newListHostedConfigurationVersionsResponse' smart constructor.
data ListHostedConfigurationVersionsResponse = ListHostedConfigurationVersionsResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [HostedConfigurationVersionSummary],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostedConfigurationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listHostedConfigurationVersionsResponse_items' - The elements from this collection.
--
-- 'nextToken', 'listHostedConfigurationVersionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listHostedConfigurationVersionsResponse_httpStatus' - The response's http status code.
newListHostedConfigurationVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHostedConfigurationVersionsResponse
newListHostedConfigurationVersionsResponse
  pHttpStatus_ =
    ListHostedConfigurationVersionsResponse'
      { items =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The elements from this collection.
listHostedConfigurationVersionsResponse_items :: Lens.Lens' ListHostedConfigurationVersionsResponse (Prelude.Maybe [HostedConfigurationVersionSummary])
listHostedConfigurationVersionsResponse_items = Lens.lens (\ListHostedConfigurationVersionsResponse' {items} -> items) (\s@ListHostedConfigurationVersionsResponse' {} a -> s {items = a} :: ListHostedConfigurationVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listHostedConfigurationVersionsResponse_nextToken :: Lens.Lens' ListHostedConfigurationVersionsResponse (Prelude.Maybe Prelude.Text)
listHostedConfigurationVersionsResponse_nextToken = Lens.lens (\ListHostedConfigurationVersionsResponse' {nextToken} -> nextToken) (\s@ListHostedConfigurationVersionsResponse' {} a -> s {nextToken = a} :: ListHostedConfigurationVersionsResponse)

-- | The response's http status code.
listHostedConfigurationVersionsResponse_httpStatus :: Lens.Lens' ListHostedConfigurationVersionsResponse Prelude.Int
listHostedConfigurationVersionsResponse_httpStatus = Lens.lens (\ListHostedConfigurationVersionsResponse' {httpStatus} -> httpStatus) (\s@ListHostedConfigurationVersionsResponse' {} a -> s {httpStatus = a} :: ListHostedConfigurationVersionsResponse)

instance
  Prelude.NFData
    ListHostedConfigurationVersionsResponse
  where
  rnf ListHostedConfigurationVersionsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
