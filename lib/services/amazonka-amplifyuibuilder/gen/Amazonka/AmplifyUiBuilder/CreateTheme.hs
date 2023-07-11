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
-- Module      : Amazonka.AmplifyUiBuilder.CreateTheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a theme to apply to the components in an Amplify app.
module Amazonka.AmplifyUiBuilder.CreateTheme
  ( -- * Creating a Request
    CreateTheme (..),
    newCreateTheme,

    -- * Request Lenses
    createTheme_clientToken,
    createTheme_appId,
    createTheme_environmentName,
    createTheme_themeToCreate,

    -- * Destructuring the Response
    CreateThemeResponse (..),
    newCreateThemeResponse,

    -- * Response Lenses
    createThemeResponse_entity,
    createThemeResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTheme' smart constructor.
data CreateTheme = CreateTheme'
  { -- | The unique client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Amplify app associated with the theme.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | Represents the configuration of the theme to create.
    themeToCreate :: CreateThemeData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTheme_clientToken' - The unique client token.
--
-- 'appId', 'createTheme_appId' - The unique ID of the Amplify app associated with the theme.
--
-- 'environmentName', 'createTheme_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'themeToCreate', 'createTheme_themeToCreate' - Represents the configuration of the theme to create.
newCreateTheme ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'themeToCreate'
  CreateThemeData ->
  CreateTheme
newCreateTheme
  pAppId_
  pEnvironmentName_
  pThemeToCreate_ =
    CreateTheme'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        themeToCreate = pThemeToCreate_
      }

-- | The unique client token.
createTheme_clientToken :: Lens.Lens' CreateTheme (Prelude.Maybe Prelude.Text)
createTheme_clientToken = Lens.lens (\CreateTheme' {clientToken} -> clientToken) (\s@CreateTheme' {} a -> s {clientToken = a} :: CreateTheme)

-- | The unique ID of the Amplify app associated with the theme.
createTheme_appId :: Lens.Lens' CreateTheme Prelude.Text
createTheme_appId = Lens.lens (\CreateTheme' {appId} -> appId) (\s@CreateTheme' {} a -> s {appId = a} :: CreateTheme)

-- | The name of the backend environment that is a part of the Amplify app.
createTheme_environmentName :: Lens.Lens' CreateTheme Prelude.Text
createTheme_environmentName = Lens.lens (\CreateTheme' {environmentName} -> environmentName) (\s@CreateTheme' {} a -> s {environmentName = a} :: CreateTheme)

-- | Represents the configuration of the theme to create.
createTheme_themeToCreate :: Lens.Lens' CreateTheme CreateThemeData
createTheme_themeToCreate = Lens.lens (\CreateTheme' {themeToCreate} -> themeToCreate) (\s@CreateTheme' {} a -> s {themeToCreate = a} :: CreateTheme)

instance Core.AWSRequest CreateTheme where
  type AWSResponse CreateTheme = CreateThemeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThemeResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTheme where
  hashWithSalt _salt CreateTheme' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` themeToCreate

instance Prelude.NFData CreateTheme where
  rnf CreateTheme' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf themeToCreate

instance Data.ToHeaders CreateTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTheme where
  toJSON CreateTheme' {..} = Data.toJSON themeToCreate

instance Data.ToPath CreateTheme where
  toPath CreateTheme' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/themes"
      ]

instance Data.ToQuery CreateTheme where
  toQuery CreateTheme' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newCreateThemeResponse' smart constructor.
data CreateThemeResponse = CreateThemeResponse'
  { -- | Describes the configuration of the new theme.
    entity :: Prelude.Maybe Theme,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'createThemeResponse_entity' - Describes the configuration of the new theme.
--
-- 'httpStatus', 'createThemeResponse_httpStatus' - The response's http status code.
newCreateThemeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateThemeResponse
newCreateThemeResponse pHttpStatus_ =
  CreateThemeResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the configuration of the new theme.
createThemeResponse_entity :: Lens.Lens' CreateThemeResponse (Prelude.Maybe Theme)
createThemeResponse_entity = Lens.lens (\CreateThemeResponse' {entity} -> entity) (\s@CreateThemeResponse' {} a -> s {entity = a} :: CreateThemeResponse)

-- | The response's http status code.
createThemeResponse_httpStatus :: Lens.Lens' CreateThemeResponse Prelude.Int
createThemeResponse_httpStatus = Lens.lens (\CreateThemeResponse' {httpStatus} -> httpStatus) (\s@CreateThemeResponse' {} a -> s {httpStatus = a} :: CreateThemeResponse)

instance Prelude.NFData CreateThemeResponse where
  rnf CreateThemeResponse' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf httpStatus
