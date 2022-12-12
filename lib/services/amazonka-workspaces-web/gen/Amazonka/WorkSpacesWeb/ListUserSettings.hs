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
-- Module      : Amazonka.WorkSpacesWeb.ListUserSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of user settings.
module Amazonka.WorkSpacesWeb.ListUserSettings
  ( -- * Creating a Request
    ListUserSettings (..),
    newListUserSettings,

    -- * Request Lenses
    listUserSettings_maxResults,
    listUserSettings_nextToken,

    -- * Destructuring the Response
    ListUserSettingsResponse (..),
    newListUserSettingsResponse,

    -- * Response Lenses
    listUserSettingsResponse_nextToken,
    listUserSettingsResponse_userSettings,
    listUserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListUserSettings' smart constructor.
data ListUserSettings = ListUserSettings'
  { -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUserSettings_maxResults' - The maximum number of results to be included in the next page.
--
-- 'nextToken', 'listUserSettings_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newListUserSettings ::
  ListUserSettings
newListUserSettings =
  ListUserSettings'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be included in the next page.
listUserSettings_maxResults :: Lens.Lens' ListUserSettings (Prelude.Maybe Prelude.Natural)
listUserSettings_maxResults = Lens.lens (\ListUserSettings' {maxResults} -> maxResults) (\s@ListUserSettings' {} a -> s {maxResults = a} :: ListUserSettings)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listUserSettings_nextToken :: Lens.Lens' ListUserSettings (Prelude.Maybe Prelude.Text)
listUserSettings_nextToken = Lens.lens (\ListUserSettings' {nextToken} -> nextToken) (\s@ListUserSettings' {} a -> s {nextToken = a} :: ListUserSettings)

instance Core.AWSRequest ListUserSettings where
  type
    AWSResponse ListUserSettings =
      ListUserSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserSettingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "userSettings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUserSettings where
  hashWithSalt _salt ListUserSettings' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListUserSettings where
  rnf ListUserSettings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListUserSettings where
  toPath = Prelude.const "/userSettings"

instance Data.ToQuery ListUserSettings where
  toQuery ListUserSettings' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListUserSettingsResponse' smart constructor.
data ListUserSettingsResponse = ListUserSettingsResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user settings.
    userSettings :: Prelude.Maybe [UserSettingsSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserSettingsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'userSettings', 'listUserSettingsResponse_userSettings' - The user settings.
--
-- 'httpStatus', 'listUserSettingsResponse_httpStatus' - The response's http status code.
newListUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserSettingsResponse
newListUserSettingsResponse pHttpStatus_ =
  ListUserSettingsResponse'
    { nextToken =
        Prelude.Nothing,
      userSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listUserSettingsResponse_nextToken :: Lens.Lens' ListUserSettingsResponse (Prelude.Maybe Prelude.Text)
listUserSettingsResponse_nextToken = Lens.lens (\ListUserSettingsResponse' {nextToken} -> nextToken) (\s@ListUserSettingsResponse' {} a -> s {nextToken = a} :: ListUserSettingsResponse)

-- | The user settings.
listUserSettingsResponse_userSettings :: Lens.Lens' ListUserSettingsResponse (Prelude.Maybe [UserSettingsSummary])
listUserSettingsResponse_userSettings = Lens.lens (\ListUserSettingsResponse' {userSettings} -> userSettings) (\s@ListUserSettingsResponse' {} a -> s {userSettings = a} :: ListUserSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUserSettingsResponse_httpStatus :: Lens.Lens' ListUserSettingsResponse Prelude.Int
listUserSettingsResponse_httpStatus = Lens.lens (\ListUserSettingsResponse' {httpStatus} -> httpStatus) (\s@ListUserSettingsResponse' {} a -> s {httpStatus = a} :: ListUserSettingsResponse)

instance Prelude.NFData ListUserSettingsResponse where
  rnf ListUserSettingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf httpStatus
