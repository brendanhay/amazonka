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
-- Module      : Amazonka.WorkSpacesWeb.ListIpAccessSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of IP access settings.
module Amazonka.WorkSpacesWeb.ListIpAccessSettings
  ( -- * Creating a Request
    ListIpAccessSettings (..),
    newListIpAccessSettings,

    -- * Request Lenses
    listIpAccessSettings_maxResults,
    listIpAccessSettings_nextToken,

    -- * Destructuring the Response
    ListIpAccessSettingsResponse (..),
    newListIpAccessSettingsResponse,

    -- * Response Lenses
    listIpAccessSettingsResponse_ipAccessSettings,
    listIpAccessSettingsResponse_nextToken,
    listIpAccessSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListIpAccessSettings' smart constructor.
data ListIpAccessSettings = ListIpAccessSettings'
  { -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIpAccessSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIpAccessSettings_maxResults' - The maximum number of results to be included in the next page.
--
-- 'nextToken', 'listIpAccessSettings_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newListIpAccessSettings ::
  ListIpAccessSettings
newListIpAccessSettings =
  ListIpAccessSettings'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be included in the next page.
listIpAccessSettings_maxResults :: Lens.Lens' ListIpAccessSettings (Prelude.Maybe Prelude.Natural)
listIpAccessSettings_maxResults = Lens.lens (\ListIpAccessSettings' {maxResults} -> maxResults) (\s@ListIpAccessSettings' {} a -> s {maxResults = a} :: ListIpAccessSettings)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listIpAccessSettings_nextToken :: Lens.Lens' ListIpAccessSettings (Prelude.Maybe Prelude.Text)
listIpAccessSettings_nextToken = Lens.lens (\ListIpAccessSettings' {nextToken} -> nextToken) (\s@ListIpAccessSettings' {} a -> s {nextToken = a} :: ListIpAccessSettings)

instance Core.AWSRequest ListIpAccessSettings where
  type
    AWSResponse ListIpAccessSettings =
      ListIpAccessSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIpAccessSettingsResponse'
            Prelude.<$> ( x
                            Data..?> "ipAccessSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIpAccessSettings where
  hashWithSalt _salt ListIpAccessSettings' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListIpAccessSettings where
  rnf ListIpAccessSettings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListIpAccessSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIpAccessSettings where
  toPath = Prelude.const "/ipAccessSettings"

instance Data.ToQuery ListIpAccessSettings where
  toQuery ListIpAccessSettings' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListIpAccessSettingsResponse' smart constructor.
data ListIpAccessSettingsResponse = ListIpAccessSettingsResponse'
  { -- | The IP access settings.
    ipAccessSettings :: Prelude.Maybe [IpAccessSettingsSummary],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIpAccessSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAccessSettings', 'listIpAccessSettingsResponse_ipAccessSettings' - The IP access settings.
--
-- 'nextToken', 'listIpAccessSettingsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'httpStatus', 'listIpAccessSettingsResponse_httpStatus' - The response's http status code.
newListIpAccessSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIpAccessSettingsResponse
newListIpAccessSettingsResponse pHttpStatus_ =
  ListIpAccessSettingsResponse'
    { ipAccessSettings =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IP access settings.
listIpAccessSettingsResponse_ipAccessSettings :: Lens.Lens' ListIpAccessSettingsResponse (Prelude.Maybe [IpAccessSettingsSummary])
listIpAccessSettingsResponse_ipAccessSettings = Lens.lens (\ListIpAccessSettingsResponse' {ipAccessSettings} -> ipAccessSettings) (\s@ListIpAccessSettingsResponse' {} a -> s {ipAccessSettings = a} :: ListIpAccessSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listIpAccessSettingsResponse_nextToken :: Lens.Lens' ListIpAccessSettingsResponse (Prelude.Maybe Prelude.Text)
listIpAccessSettingsResponse_nextToken = Lens.lens (\ListIpAccessSettingsResponse' {nextToken} -> nextToken) (\s@ListIpAccessSettingsResponse' {} a -> s {nextToken = a} :: ListIpAccessSettingsResponse)

-- | The response's http status code.
listIpAccessSettingsResponse_httpStatus :: Lens.Lens' ListIpAccessSettingsResponse Prelude.Int
listIpAccessSettingsResponse_httpStatus = Lens.lens (\ListIpAccessSettingsResponse' {httpStatus} -> httpStatus) (\s@ListIpAccessSettingsResponse' {} a -> s {httpStatus = a} :: ListIpAccessSettingsResponse)

instance Prelude.NFData ListIpAccessSettingsResponse where
  rnf ListIpAccessSettingsResponse' {..} =
    Prelude.rnf ipAccessSettings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
