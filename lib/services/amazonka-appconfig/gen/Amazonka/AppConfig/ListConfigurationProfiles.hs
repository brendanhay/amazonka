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
-- Module      : Amazonka.AppConfig.ListConfigurationProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configuration profiles for an application.
module Amazonka.AppConfig.ListConfigurationProfiles
  ( -- * Creating a Request
    ListConfigurationProfiles (..),
    newListConfigurationProfiles,

    -- * Request Lenses
    listConfigurationProfiles_maxResults,
    listConfigurationProfiles_nextToken,
    listConfigurationProfiles_type,
    listConfigurationProfiles_applicationId,

    -- * Destructuring the Response
    ListConfigurationProfilesResponse (..),
    newListConfigurationProfilesResponse,

    -- * Response Lenses
    listConfigurationProfilesResponse_items,
    listConfigurationProfilesResponse_nextToken,
    listConfigurationProfilesResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConfigurationProfiles' smart constructor.
data ListConfigurationProfiles = ListConfigurationProfiles'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter based on the type of configurations that the configuration
    -- profile contains. A configuration can be a feature flag or a freeform
    -- configuration.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The application ID.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConfigurationProfiles_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listConfigurationProfiles_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'type'', 'listConfigurationProfiles_type' - A filter based on the type of configurations that the configuration
-- profile contains. A configuration can be a feature flag or a freeform
-- configuration.
--
-- 'applicationId', 'listConfigurationProfiles_applicationId' - The application ID.
newListConfigurationProfiles ::
  -- | 'applicationId'
  Prelude.Text ->
  ListConfigurationProfiles
newListConfigurationProfiles pApplicationId_ =
  ListConfigurationProfiles'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listConfigurationProfiles_maxResults :: Lens.Lens' ListConfigurationProfiles (Prelude.Maybe Prelude.Natural)
listConfigurationProfiles_maxResults = Lens.lens (\ListConfigurationProfiles' {maxResults} -> maxResults) (\s@ListConfigurationProfiles' {} a -> s {maxResults = a} :: ListConfigurationProfiles)

-- | A token to start the list. Use this token to get the next set of
-- results.
listConfigurationProfiles_nextToken :: Lens.Lens' ListConfigurationProfiles (Prelude.Maybe Prelude.Text)
listConfigurationProfiles_nextToken = Lens.lens (\ListConfigurationProfiles' {nextToken} -> nextToken) (\s@ListConfigurationProfiles' {} a -> s {nextToken = a} :: ListConfigurationProfiles)

-- | A filter based on the type of configurations that the configuration
-- profile contains. A configuration can be a feature flag or a freeform
-- configuration.
listConfigurationProfiles_type :: Lens.Lens' ListConfigurationProfiles (Prelude.Maybe Prelude.Text)
listConfigurationProfiles_type = Lens.lens (\ListConfigurationProfiles' {type'} -> type') (\s@ListConfigurationProfiles' {} a -> s {type' = a} :: ListConfigurationProfiles)

-- | The application ID.
listConfigurationProfiles_applicationId :: Lens.Lens' ListConfigurationProfiles Prelude.Text
listConfigurationProfiles_applicationId = Lens.lens (\ListConfigurationProfiles' {applicationId} -> applicationId) (\s@ListConfigurationProfiles' {} a -> s {applicationId = a} :: ListConfigurationProfiles)

instance Core.AWSRequest ListConfigurationProfiles where
  type
    AWSResponse ListConfigurationProfiles =
      ListConfigurationProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationProfilesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationProfiles where
  hashWithSalt _salt ListConfigurationProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListConfigurationProfiles where
  rnf ListConfigurationProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListConfigurationProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConfigurationProfiles where
  toPath ListConfigurationProfiles' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/configurationprofiles"
      ]

instance Data.ToQuery ListConfigurationProfiles where
  toQuery ListConfigurationProfiles' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "next_token" Data.=: nextToken,
        "type" Data.=: type'
      ]

-- | /See:/ 'newListConfigurationProfilesResponse' smart constructor.
data ListConfigurationProfilesResponse = ListConfigurationProfilesResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [ConfigurationProfileSummary],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listConfigurationProfilesResponse_items' - The elements from this collection.
--
-- 'nextToken', 'listConfigurationProfilesResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listConfigurationProfilesResponse_httpStatus' - The response's http status code.
newListConfigurationProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationProfilesResponse
newListConfigurationProfilesResponse pHttpStatus_ =
  ListConfigurationProfilesResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
listConfigurationProfilesResponse_items :: Lens.Lens' ListConfigurationProfilesResponse (Prelude.Maybe [ConfigurationProfileSummary])
listConfigurationProfilesResponse_items = Lens.lens (\ListConfigurationProfilesResponse' {items} -> items) (\s@ListConfigurationProfilesResponse' {} a -> s {items = a} :: ListConfigurationProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listConfigurationProfilesResponse_nextToken :: Lens.Lens' ListConfigurationProfilesResponse (Prelude.Maybe Prelude.Text)
listConfigurationProfilesResponse_nextToken = Lens.lens (\ListConfigurationProfilesResponse' {nextToken} -> nextToken) (\s@ListConfigurationProfilesResponse' {} a -> s {nextToken = a} :: ListConfigurationProfilesResponse)

-- | The response's http status code.
listConfigurationProfilesResponse_httpStatus :: Lens.Lens' ListConfigurationProfilesResponse Prelude.Int
listConfigurationProfilesResponse_httpStatus = Lens.lens (\ListConfigurationProfilesResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationProfilesResponse' {} a -> s {httpStatus = a} :: ListConfigurationProfilesResponse)

instance
  Prelude.NFData
    ListConfigurationProfilesResponse
  where
  rnf ListConfigurationProfilesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
