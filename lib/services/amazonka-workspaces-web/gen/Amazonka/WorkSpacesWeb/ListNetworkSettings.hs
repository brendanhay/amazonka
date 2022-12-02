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
-- Module      : Amazonka.WorkSpacesWeb.ListNetworkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of network settings.
module Amazonka.WorkSpacesWeb.ListNetworkSettings
  ( -- * Creating a Request
    ListNetworkSettings (..),
    newListNetworkSettings,

    -- * Request Lenses
    listNetworkSettings_nextToken,
    listNetworkSettings_maxResults,

    -- * Destructuring the Response
    ListNetworkSettingsResponse (..),
    newListNetworkSettingsResponse,

    -- * Response Lenses
    listNetworkSettingsResponse_nextToken,
    listNetworkSettingsResponse_networkSettings,
    listNetworkSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListNetworkSettings' smart constructor.
data ListNetworkSettings = ListNetworkSettings'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkSettings_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'maxResults', 'listNetworkSettings_maxResults' - The maximum number of results to be included in the next page.
newListNetworkSettings ::
  ListNetworkSettings
newListNetworkSettings =
  ListNetworkSettings'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listNetworkSettings_nextToken :: Lens.Lens' ListNetworkSettings (Prelude.Maybe Prelude.Text)
listNetworkSettings_nextToken = Lens.lens (\ListNetworkSettings' {nextToken} -> nextToken) (\s@ListNetworkSettings' {} a -> s {nextToken = a} :: ListNetworkSettings)

-- | The maximum number of results to be included in the next page.
listNetworkSettings_maxResults :: Lens.Lens' ListNetworkSettings (Prelude.Maybe Prelude.Natural)
listNetworkSettings_maxResults = Lens.lens (\ListNetworkSettings' {maxResults} -> maxResults) (\s@ListNetworkSettings' {} a -> s {maxResults = a} :: ListNetworkSettings)

instance Core.AWSRequest ListNetworkSettings where
  type
    AWSResponse ListNetworkSettings =
      ListNetworkSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworkSettingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "networkSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworkSettings where
  hashWithSalt _salt ListNetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListNetworkSettings where
  rnf ListNetworkSettings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListNetworkSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListNetworkSettings where
  toPath = Prelude.const "/networkSettings"

instance Data.ToQuery ListNetworkSettings where
  toQuery ListNetworkSettings' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListNetworkSettingsResponse' smart constructor.
data ListNetworkSettingsResponse = ListNetworkSettingsResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The network settings.
    networkSettings :: Prelude.Maybe [NetworkSettingsSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkSettingsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'networkSettings', 'listNetworkSettingsResponse_networkSettings' - The network settings.
--
-- 'httpStatus', 'listNetworkSettingsResponse_httpStatus' - The response's http status code.
newListNetworkSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworkSettingsResponse
newListNetworkSettingsResponse pHttpStatus_ =
  ListNetworkSettingsResponse'
    { nextToken =
        Prelude.Nothing,
      networkSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listNetworkSettingsResponse_nextToken :: Lens.Lens' ListNetworkSettingsResponse (Prelude.Maybe Prelude.Text)
listNetworkSettingsResponse_nextToken = Lens.lens (\ListNetworkSettingsResponse' {nextToken} -> nextToken) (\s@ListNetworkSettingsResponse' {} a -> s {nextToken = a} :: ListNetworkSettingsResponse)

-- | The network settings.
listNetworkSettingsResponse_networkSettings :: Lens.Lens' ListNetworkSettingsResponse (Prelude.Maybe [NetworkSettingsSummary])
listNetworkSettingsResponse_networkSettings = Lens.lens (\ListNetworkSettingsResponse' {networkSettings} -> networkSettings) (\s@ListNetworkSettingsResponse' {} a -> s {networkSettings = a} :: ListNetworkSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNetworkSettingsResponse_httpStatus :: Lens.Lens' ListNetworkSettingsResponse Prelude.Int
listNetworkSettingsResponse_httpStatus = Lens.lens (\ListNetworkSettingsResponse' {httpStatus} -> httpStatus) (\s@ListNetworkSettingsResponse' {} a -> s {httpStatus = a} :: ListNetworkSettingsResponse)

instance Prelude.NFData ListNetworkSettingsResponse where
  rnf ListNetworkSettingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf networkSettings
      `Prelude.seq` Prelude.rnf httpStatus
