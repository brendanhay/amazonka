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
-- Module      : Network.AWS.ApiGatewayV2.DeleteRouteSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the RouteSettings for a stage.
module Network.AWS.ApiGatewayV2.DeleteRouteSettings
  ( -- * Creating a Request
    DeleteRouteSettings (..),
    newDeleteRouteSettings,

    -- * Request Lenses
    deleteRouteSettings_stageName,
    deleteRouteSettings_routeKey,
    deleteRouteSettings_apiId,

    -- * Destructuring the Response
    DeleteRouteSettingsResponse (..),
    newDeleteRouteSettingsResponse,
  )
where

import Network.AWS.ApiGatewayV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRouteSettings' smart constructor.
data DeleteRouteSettings = DeleteRouteSettings'
  { -- | The stage name. Stage names can only contain alphanumeric characters,
    -- hyphens, and underscores. Maximum length is 128 characters.
    stageName :: Prelude.Text,
    -- | The route key.
    routeKey :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'deleteRouteSettings_stageName' - The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
--
-- 'routeKey', 'deleteRouteSettings_routeKey' - The route key.
--
-- 'apiId', 'deleteRouteSettings_apiId' - The API identifier.
newDeleteRouteSettings ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'routeKey'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  DeleteRouteSettings
newDeleteRouteSettings pStageName_ pRouteKey_ pApiId_ =
  DeleteRouteSettings'
    { stageName = pStageName_,
      routeKey = pRouteKey_,
      apiId = pApiId_
    }

-- | The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
deleteRouteSettings_stageName :: Lens.Lens' DeleteRouteSettings Prelude.Text
deleteRouteSettings_stageName = Lens.lens (\DeleteRouteSettings' {stageName} -> stageName) (\s@DeleteRouteSettings' {} a -> s {stageName = a} :: DeleteRouteSettings)

-- | The route key.
deleteRouteSettings_routeKey :: Lens.Lens' DeleteRouteSettings Prelude.Text
deleteRouteSettings_routeKey = Lens.lens (\DeleteRouteSettings' {routeKey} -> routeKey) (\s@DeleteRouteSettings' {} a -> s {routeKey = a} :: DeleteRouteSettings)

-- | The API identifier.
deleteRouteSettings_apiId :: Lens.Lens' DeleteRouteSettings Prelude.Text
deleteRouteSettings_apiId = Lens.lens (\DeleteRouteSettings' {apiId} -> apiId) (\s@DeleteRouteSettings' {} a -> s {apiId = a} :: DeleteRouteSettings)

instance Core.AWSRequest DeleteRouteSettings where
  type
    AWSResponse DeleteRouteSettings =
      DeleteRouteSettingsResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteRouteSettingsResponse'

instance Prelude.Hashable DeleteRouteSettings

instance Prelude.NFData DeleteRouteSettings

instance Core.ToHeaders DeleteRouteSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteRouteSettings where
  toPath DeleteRouteSettings' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/stages/",
        Core.toBS stageName,
        "/routesettings/",
        Core.toBS routeKey
      ]

instance Core.ToQuery DeleteRouteSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRouteSettingsResponse' smart constructor.
data DeleteRouteSettingsResponse = DeleteRouteSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteSettingsResponse ::
  DeleteRouteSettingsResponse
newDeleteRouteSettingsResponse =
  DeleteRouteSettingsResponse'

instance Prelude.NFData DeleteRouteSettingsResponse
