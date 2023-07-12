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
-- Module      : Amazonka.ApiGatewayV2.DeleteAccessLogSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the AccessLogSettings for a Stage. To disable access logging for
-- a Stage, delete its AccessLogSettings.
module Amazonka.ApiGatewayV2.DeleteAccessLogSettings
  ( -- * Creating a Request
    DeleteAccessLogSettings (..),
    newDeleteAccessLogSettings,

    -- * Request Lenses
    deleteAccessLogSettings_stageName,
    deleteAccessLogSettings_apiId,

    -- * Destructuring the Response
    DeleteAccessLogSettingsResponse (..),
    newDeleteAccessLogSettingsResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccessLogSettings' smart constructor.
data DeleteAccessLogSettings = DeleteAccessLogSettings'
  { -- | The stage name. Stage names can only contain alphanumeric characters,
    -- hyphens, and underscores. Maximum length is 128 characters.
    stageName :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessLogSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'deleteAccessLogSettings_stageName' - The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
--
-- 'apiId', 'deleteAccessLogSettings_apiId' - The API identifier.
newDeleteAccessLogSettings ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  DeleteAccessLogSettings
newDeleteAccessLogSettings pStageName_ pApiId_ =
  DeleteAccessLogSettings'
    { stageName = pStageName_,
      apiId = pApiId_
    }

-- | The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
deleteAccessLogSettings_stageName :: Lens.Lens' DeleteAccessLogSettings Prelude.Text
deleteAccessLogSettings_stageName = Lens.lens (\DeleteAccessLogSettings' {stageName} -> stageName) (\s@DeleteAccessLogSettings' {} a -> s {stageName = a} :: DeleteAccessLogSettings)

-- | The API identifier.
deleteAccessLogSettings_apiId :: Lens.Lens' DeleteAccessLogSettings Prelude.Text
deleteAccessLogSettings_apiId = Lens.lens (\DeleteAccessLogSettings' {apiId} -> apiId) (\s@DeleteAccessLogSettings' {} a -> s {apiId = a} :: DeleteAccessLogSettings)

instance Core.AWSRequest DeleteAccessLogSettings where
  type
    AWSResponse DeleteAccessLogSettings =
      DeleteAccessLogSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAccessLogSettingsResponse'

instance Prelude.Hashable DeleteAccessLogSettings where
  hashWithSalt _salt DeleteAccessLogSettings' {..} =
    _salt
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData DeleteAccessLogSettings where
  rnf DeleteAccessLogSettings' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders DeleteAccessLogSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAccessLogSettings where
  toPath DeleteAccessLogSettings' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/stages/",
        Data.toBS stageName,
        "/accesslogsettings"
      ]

instance Data.ToQuery DeleteAccessLogSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessLogSettingsResponse' smart constructor.
data DeleteAccessLogSettingsResponse = DeleteAccessLogSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessLogSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccessLogSettingsResponse ::
  DeleteAccessLogSettingsResponse
newDeleteAccessLogSettingsResponse =
  DeleteAccessLogSettingsResponse'

instance
  Prelude.NFData
    DeleteAccessLogSettingsResponse
  where
  rnf _ = ()
