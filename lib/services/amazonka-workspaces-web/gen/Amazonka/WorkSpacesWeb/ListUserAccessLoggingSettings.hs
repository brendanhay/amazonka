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
-- Module      : Amazonka.WorkSpacesWeb.ListUserAccessLoggingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of user access logging settings.
module Amazonka.WorkSpacesWeb.ListUserAccessLoggingSettings
  ( -- * Creating a Request
    ListUserAccessLoggingSettings (..),
    newListUserAccessLoggingSettings,

    -- * Request Lenses
    listUserAccessLoggingSettings_maxResults,
    listUserAccessLoggingSettings_nextToken,

    -- * Destructuring the Response
    ListUserAccessLoggingSettingsResponse (..),
    newListUserAccessLoggingSettingsResponse,

    -- * Response Lenses
    listUserAccessLoggingSettingsResponse_nextToken,
    listUserAccessLoggingSettingsResponse_userAccessLoggingSettings,
    listUserAccessLoggingSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListUserAccessLoggingSettings' smart constructor.
data ListUserAccessLoggingSettings = ListUserAccessLoggingSettings'
  { -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserAccessLoggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUserAccessLoggingSettings_maxResults' - The maximum number of results to be included in the next page.
--
-- 'nextToken', 'listUserAccessLoggingSettings_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newListUserAccessLoggingSettings ::
  ListUserAccessLoggingSettings
newListUserAccessLoggingSettings =
  ListUserAccessLoggingSettings'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be included in the next page.
listUserAccessLoggingSettings_maxResults :: Lens.Lens' ListUserAccessLoggingSettings (Prelude.Maybe Prelude.Natural)
listUserAccessLoggingSettings_maxResults = Lens.lens (\ListUserAccessLoggingSettings' {maxResults} -> maxResults) (\s@ListUserAccessLoggingSettings' {} a -> s {maxResults = a} :: ListUserAccessLoggingSettings)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listUserAccessLoggingSettings_nextToken :: Lens.Lens' ListUserAccessLoggingSettings (Prelude.Maybe Prelude.Text)
listUserAccessLoggingSettings_nextToken = Lens.lens (\ListUserAccessLoggingSettings' {nextToken} -> nextToken) (\s@ListUserAccessLoggingSettings' {} a -> s {nextToken = a} :: ListUserAccessLoggingSettings)

instance
  Core.AWSRequest
    ListUserAccessLoggingSettings
  where
  type
    AWSResponse ListUserAccessLoggingSettings =
      ListUserAccessLoggingSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserAccessLoggingSettingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "userAccessLoggingSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListUserAccessLoggingSettings
  where
  hashWithSalt _salt ListUserAccessLoggingSettings' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListUserAccessLoggingSettings where
  rnf ListUserAccessLoggingSettings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListUserAccessLoggingSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListUserAccessLoggingSettings where
  toPath = Prelude.const "/userAccessLoggingSettings"

instance Data.ToQuery ListUserAccessLoggingSettings where
  toQuery ListUserAccessLoggingSettings' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListUserAccessLoggingSettingsResponse' smart constructor.
data ListUserAccessLoggingSettingsResponse = ListUserAccessLoggingSettingsResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user access logging settings.
    userAccessLoggingSettings :: Prelude.Maybe [UserAccessLoggingSettingsSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserAccessLoggingSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserAccessLoggingSettingsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'userAccessLoggingSettings', 'listUserAccessLoggingSettingsResponse_userAccessLoggingSettings' - The user access logging settings.
--
-- 'httpStatus', 'listUserAccessLoggingSettingsResponse_httpStatus' - The response's http status code.
newListUserAccessLoggingSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserAccessLoggingSettingsResponse
newListUserAccessLoggingSettingsResponse pHttpStatus_ =
  ListUserAccessLoggingSettingsResponse'
    { nextToken =
        Prelude.Nothing,
      userAccessLoggingSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listUserAccessLoggingSettingsResponse_nextToken :: Lens.Lens' ListUserAccessLoggingSettingsResponse (Prelude.Maybe Prelude.Text)
listUserAccessLoggingSettingsResponse_nextToken = Lens.lens (\ListUserAccessLoggingSettingsResponse' {nextToken} -> nextToken) (\s@ListUserAccessLoggingSettingsResponse' {} a -> s {nextToken = a} :: ListUserAccessLoggingSettingsResponse)

-- | The user access logging settings.
listUserAccessLoggingSettingsResponse_userAccessLoggingSettings :: Lens.Lens' ListUserAccessLoggingSettingsResponse (Prelude.Maybe [UserAccessLoggingSettingsSummary])
listUserAccessLoggingSettingsResponse_userAccessLoggingSettings = Lens.lens (\ListUserAccessLoggingSettingsResponse' {userAccessLoggingSettings} -> userAccessLoggingSettings) (\s@ListUserAccessLoggingSettingsResponse' {} a -> s {userAccessLoggingSettings = a} :: ListUserAccessLoggingSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUserAccessLoggingSettingsResponse_httpStatus :: Lens.Lens' ListUserAccessLoggingSettingsResponse Prelude.Int
listUserAccessLoggingSettingsResponse_httpStatus = Lens.lens (\ListUserAccessLoggingSettingsResponse' {httpStatus} -> httpStatus) (\s@ListUserAccessLoggingSettingsResponse' {} a -> s {httpStatus = a} :: ListUserAccessLoggingSettingsResponse)

instance
  Prelude.NFData
    ListUserAccessLoggingSettingsResponse
  where
  rnf ListUserAccessLoggingSettingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userAccessLoggingSettings
      `Prelude.seq` Prelude.rnf httpStatus
