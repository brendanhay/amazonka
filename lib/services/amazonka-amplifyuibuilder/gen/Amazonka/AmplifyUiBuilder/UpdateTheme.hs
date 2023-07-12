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
-- Module      : Amazonka.AmplifyUiBuilder.UpdateTheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing theme.
module Amazonka.AmplifyUiBuilder.UpdateTheme
  ( -- * Creating a Request
    UpdateTheme (..),
    newUpdateTheme,

    -- * Request Lenses
    updateTheme_clientToken,
    updateTheme_appId,
    updateTheme_environmentName,
    updateTheme_id,
    updateTheme_updatedTheme,

    -- * Destructuring the Response
    UpdateThemeResponse (..),
    newUpdateThemeResponse,

    -- * Response Lenses
    updateThemeResponse_entity,
    updateThemeResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTheme' smart constructor.
data UpdateTheme = UpdateTheme'
  { -- | The unique client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID for the theme.
    id :: Prelude.Text,
    -- | The configuration of the updated theme.
    updatedTheme :: UpdateThemeData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateTheme_clientToken' - The unique client token.
--
-- 'appId', 'updateTheme_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'updateTheme_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'updateTheme_id' - The unique ID for the theme.
--
-- 'updatedTheme', 'updateTheme_updatedTheme' - The configuration of the updated theme.
newUpdateTheme ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'updatedTheme'
  UpdateThemeData ->
  UpdateTheme
newUpdateTheme
  pAppId_
  pEnvironmentName_
  pId_
  pUpdatedTheme_ =
    UpdateTheme'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        id = pId_,
        updatedTheme = pUpdatedTheme_
      }

-- | The unique client token.
updateTheme_clientToken :: Lens.Lens' UpdateTheme (Prelude.Maybe Prelude.Text)
updateTheme_clientToken = Lens.lens (\UpdateTheme' {clientToken} -> clientToken) (\s@UpdateTheme' {} a -> s {clientToken = a} :: UpdateTheme)

-- | The unique ID for the Amplify app.
updateTheme_appId :: Lens.Lens' UpdateTheme Prelude.Text
updateTheme_appId = Lens.lens (\UpdateTheme' {appId} -> appId) (\s@UpdateTheme' {} a -> s {appId = a} :: UpdateTheme)

-- | The name of the backend environment that is part of the Amplify app.
updateTheme_environmentName :: Lens.Lens' UpdateTheme Prelude.Text
updateTheme_environmentName = Lens.lens (\UpdateTheme' {environmentName} -> environmentName) (\s@UpdateTheme' {} a -> s {environmentName = a} :: UpdateTheme)

-- | The unique ID for the theme.
updateTheme_id :: Lens.Lens' UpdateTheme Prelude.Text
updateTheme_id = Lens.lens (\UpdateTheme' {id} -> id) (\s@UpdateTheme' {} a -> s {id = a} :: UpdateTheme)

-- | The configuration of the updated theme.
updateTheme_updatedTheme :: Lens.Lens' UpdateTheme UpdateThemeData
updateTheme_updatedTheme = Lens.lens (\UpdateTheme' {updatedTheme} -> updatedTheme) (\s@UpdateTheme' {} a -> s {updatedTheme = a} :: UpdateTheme)

instance Core.AWSRequest UpdateTheme where
  type AWSResponse UpdateTheme = UpdateThemeResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateThemeResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTheme where
  hashWithSalt _salt UpdateTheme' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` updatedTheme

instance Prelude.NFData UpdateTheme where
  rnf UpdateTheme' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf updatedTheme

instance Data.ToHeaders UpdateTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTheme where
  toJSON UpdateTheme' {..} = Data.toJSON updatedTheme

instance Data.ToPath UpdateTheme where
  toPath UpdateTheme' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/themes/",
        Data.toBS id
      ]

instance Data.ToQuery UpdateTheme where
  toQuery UpdateTheme' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newUpdateThemeResponse' smart constructor.
data UpdateThemeResponse = UpdateThemeResponse'
  { -- | Describes the configuration of the updated theme.
    entity :: Prelude.Maybe Theme,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'updateThemeResponse_entity' - Describes the configuration of the updated theme.
--
-- 'httpStatus', 'updateThemeResponse_httpStatus' - The response's http status code.
newUpdateThemeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateThemeResponse
newUpdateThemeResponse pHttpStatus_ =
  UpdateThemeResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the configuration of the updated theme.
updateThemeResponse_entity :: Lens.Lens' UpdateThemeResponse (Prelude.Maybe Theme)
updateThemeResponse_entity = Lens.lens (\UpdateThemeResponse' {entity} -> entity) (\s@UpdateThemeResponse' {} a -> s {entity = a} :: UpdateThemeResponse)

-- | The response's http status code.
updateThemeResponse_httpStatus :: Lens.Lens' UpdateThemeResponse Prelude.Int
updateThemeResponse_httpStatus = Lens.lens (\UpdateThemeResponse' {httpStatus} -> httpStatus) (\s@UpdateThemeResponse' {} a -> s {httpStatus = a} :: UpdateThemeResponse)

instance Prelude.NFData UpdateThemeResponse where
  rnf UpdateThemeResponse' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf httpStatus
