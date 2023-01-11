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
-- Module      : Amazonka.AmplifyUiBuilder.GetTheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an existing theme for an Amplify app.
module Amazonka.AmplifyUiBuilder.GetTheme
  ( -- * Creating a Request
    GetTheme (..),
    newGetTheme,

    -- * Request Lenses
    getTheme_appId,
    getTheme_environmentName,
    getTheme_id,

    -- * Destructuring the Response
    GetThemeResponse (..),
    newGetThemeResponse,

    -- * Response Lenses
    getThemeResponse_theme,
    getThemeResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTheme' smart constructor.
data GetTheme = GetTheme'
  { -- | The unique ID of the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID for the theme.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getTheme_appId' - The unique ID of the Amplify app.
--
-- 'environmentName', 'getTheme_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'getTheme_id' - The unique ID for the theme.
newGetTheme ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetTheme
newGetTheme pAppId_ pEnvironmentName_ pId_ =
  GetTheme'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app.
getTheme_appId :: Lens.Lens' GetTheme Prelude.Text
getTheme_appId = Lens.lens (\GetTheme' {appId} -> appId) (\s@GetTheme' {} a -> s {appId = a} :: GetTheme)

-- | The name of the backend environment that is part of the Amplify app.
getTheme_environmentName :: Lens.Lens' GetTheme Prelude.Text
getTheme_environmentName = Lens.lens (\GetTheme' {environmentName} -> environmentName) (\s@GetTheme' {} a -> s {environmentName = a} :: GetTheme)

-- | The unique ID for the theme.
getTheme_id :: Lens.Lens' GetTheme Prelude.Text
getTheme_id = Lens.lens (\GetTheme' {id} -> id) (\s@GetTheme' {} a -> s {id = a} :: GetTheme)

instance Core.AWSRequest GetTheme where
  type AWSResponse GetTheme = GetThemeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThemeResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTheme where
  hashWithSalt _salt GetTheme' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetTheme where
  rnf GetTheme' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTheme where
  toPath GetTheme' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/themes/",
        Data.toBS id
      ]

instance Data.ToQuery GetTheme where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetThemeResponse' smart constructor.
data GetThemeResponse = GetThemeResponse'
  { -- | Represents the configuration settings for the theme.
    theme :: Prelude.Maybe Theme,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'theme', 'getThemeResponse_theme' - Represents the configuration settings for the theme.
--
-- 'httpStatus', 'getThemeResponse_httpStatus' - The response's http status code.
newGetThemeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetThemeResponse
newGetThemeResponse pHttpStatus_ =
  GetThemeResponse'
    { theme = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the configuration settings for the theme.
getThemeResponse_theme :: Lens.Lens' GetThemeResponse (Prelude.Maybe Theme)
getThemeResponse_theme = Lens.lens (\GetThemeResponse' {theme} -> theme) (\s@GetThemeResponse' {} a -> s {theme = a} :: GetThemeResponse)

-- | The response's http status code.
getThemeResponse_httpStatus :: Lens.Lens' GetThemeResponse Prelude.Int
getThemeResponse_httpStatus = Lens.lens (\GetThemeResponse' {httpStatus} -> httpStatus) (\s@GetThemeResponse' {} a -> s {httpStatus = a} :: GetThemeResponse)

instance Prelude.NFData GetThemeResponse where
  rnf GetThemeResponse' {..} =
    Prelude.rnf theme
      `Prelude.seq` Prelude.rnf httpStatus
