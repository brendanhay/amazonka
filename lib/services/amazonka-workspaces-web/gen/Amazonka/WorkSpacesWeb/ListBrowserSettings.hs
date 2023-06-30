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
-- Module      : Amazonka.WorkSpacesWeb.ListBrowserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of browser settings.
module Amazonka.WorkSpacesWeb.ListBrowserSettings
  ( -- * Creating a Request
    ListBrowserSettings (..),
    newListBrowserSettings,

    -- * Request Lenses
    listBrowserSettings_maxResults,
    listBrowserSettings_nextToken,

    -- * Destructuring the Response
    ListBrowserSettingsResponse (..),
    newListBrowserSettingsResponse,

    -- * Response Lenses
    listBrowserSettingsResponse_browserSettings,
    listBrowserSettingsResponse_nextToken,
    listBrowserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListBrowserSettings' smart constructor.
data ListBrowserSettings = ListBrowserSettings'
  { -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBrowserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBrowserSettings_maxResults' - The maximum number of results to be included in the next page.
--
-- 'nextToken', 'listBrowserSettings_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newListBrowserSettings ::
  ListBrowserSettings
newListBrowserSettings =
  ListBrowserSettings'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be included in the next page.
listBrowserSettings_maxResults :: Lens.Lens' ListBrowserSettings (Prelude.Maybe Prelude.Natural)
listBrowserSettings_maxResults = Lens.lens (\ListBrowserSettings' {maxResults} -> maxResults) (\s@ListBrowserSettings' {} a -> s {maxResults = a} :: ListBrowserSettings)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listBrowserSettings_nextToken :: Lens.Lens' ListBrowserSettings (Prelude.Maybe Prelude.Text)
listBrowserSettings_nextToken = Lens.lens (\ListBrowserSettings' {nextToken} -> nextToken) (\s@ListBrowserSettings' {} a -> s {nextToken = a} :: ListBrowserSettings)

instance Core.AWSRequest ListBrowserSettings where
  type
    AWSResponse ListBrowserSettings =
      ListBrowserSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBrowserSettingsResponse'
            Prelude.<$> ( x
                            Data..?> "browserSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBrowserSettings where
  hashWithSalt _salt ListBrowserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBrowserSettings where
  rnf ListBrowserSettings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListBrowserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBrowserSettings where
  toPath = Prelude.const "/browserSettings"

instance Data.ToQuery ListBrowserSettings where
  toQuery ListBrowserSettings' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListBrowserSettingsResponse' smart constructor.
data ListBrowserSettingsResponse = ListBrowserSettingsResponse'
  { -- | The browser settings.
    browserSettings :: Prelude.Maybe [BrowserSettingsSummary],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBrowserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettings', 'listBrowserSettingsResponse_browserSettings' - The browser settings.
--
-- 'nextToken', 'listBrowserSettingsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'httpStatus', 'listBrowserSettingsResponse_httpStatus' - The response's http status code.
newListBrowserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBrowserSettingsResponse
newListBrowserSettingsResponse pHttpStatus_ =
  ListBrowserSettingsResponse'
    { browserSettings =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The browser settings.
listBrowserSettingsResponse_browserSettings :: Lens.Lens' ListBrowserSettingsResponse (Prelude.Maybe [BrowserSettingsSummary])
listBrowserSettingsResponse_browserSettings = Lens.lens (\ListBrowserSettingsResponse' {browserSettings} -> browserSettings) (\s@ListBrowserSettingsResponse' {} a -> s {browserSettings = a} :: ListBrowserSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listBrowserSettingsResponse_nextToken :: Lens.Lens' ListBrowserSettingsResponse (Prelude.Maybe Prelude.Text)
listBrowserSettingsResponse_nextToken = Lens.lens (\ListBrowserSettingsResponse' {nextToken} -> nextToken) (\s@ListBrowserSettingsResponse' {} a -> s {nextToken = a} :: ListBrowserSettingsResponse)

-- | The response's http status code.
listBrowserSettingsResponse_httpStatus :: Lens.Lens' ListBrowserSettingsResponse Prelude.Int
listBrowserSettingsResponse_httpStatus = Lens.lens (\ListBrowserSettingsResponse' {httpStatus} -> httpStatus) (\s@ListBrowserSettingsResponse' {} a -> s {httpStatus = a} :: ListBrowserSettingsResponse)

instance Prelude.NFData ListBrowserSettingsResponse where
  rnf ListBrowserSettingsResponse' {..} =
    Prelude.rnf browserSettings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
