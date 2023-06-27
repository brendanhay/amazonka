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
-- Module      : Amazonka.ResilienceHub.UpdateAppVersionAppComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Application Component in the Resilience Hub
-- application.
--
-- This API updates the Resilience Hub application draft version. To use
-- this Application Component for running assessments, you must publish the
-- Resilience Hub application using the @PublishAppVersion@ API.
module Amazonka.ResilienceHub.UpdateAppVersionAppComponent
  ( -- * Creating a Request
    UpdateAppVersionAppComponent (..),
    newUpdateAppVersionAppComponent,

    -- * Request Lenses
    updateAppVersionAppComponent_additionalInfo,
    updateAppVersionAppComponent_name,
    updateAppVersionAppComponent_type,
    updateAppVersionAppComponent_appArn,
    updateAppVersionAppComponent_id,

    -- * Destructuring the Response
    UpdateAppVersionAppComponentResponse (..),
    newUpdateAppVersionAppComponentResponse,

    -- * Response Lenses
    updateAppVersionAppComponentResponse_appComponent,
    updateAppVersionAppComponentResponse_httpStatus,
    updateAppVersionAppComponentResponse_appArn,
    updateAppVersionAppComponentResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAppVersionAppComponent' smart constructor.
data UpdateAppVersionAppComponent = UpdateAppVersionAppComponent'
  { -- | Currently, there is no supported additional information for Application
    -- Components.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The name of the Application Component.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of Application Component. For more information about the types
    -- of Application Component, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The identifier of the Application Component.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppVersionAppComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'updateAppVersionAppComponent_additionalInfo' - Currently, there is no supported additional information for Application
-- Components.
--
-- 'name', 'updateAppVersionAppComponent_name' - The name of the Application Component.
--
-- 'type'', 'updateAppVersionAppComponent_type' - The type of Application Component. For more information about the types
-- of Application Component, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
--
-- 'appArn', 'updateAppVersionAppComponent_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'id', 'updateAppVersionAppComponent_id' - The identifier of the Application Component.
newUpdateAppVersionAppComponent ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  UpdateAppVersionAppComponent
newUpdateAppVersionAppComponent pAppArn_ pId_ =
  UpdateAppVersionAppComponent'
    { additionalInfo =
        Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      appArn = pAppArn_,
      id = pId_
    }

-- | Currently, there is no supported additional information for Application
-- Components.
updateAppVersionAppComponent_additionalInfo :: Lens.Lens' UpdateAppVersionAppComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
updateAppVersionAppComponent_additionalInfo = Lens.lens (\UpdateAppVersionAppComponent' {additionalInfo} -> additionalInfo) (\s@UpdateAppVersionAppComponent' {} a -> s {additionalInfo = a} :: UpdateAppVersionAppComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Application Component.
updateAppVersionAppComponent_name :: Lens.Lens' UpdateAppVersionAppComponent (Prelude.Maybe Prelude.Text)
updateAppVersionAppComponent_name = Lens.lens (\UpdateAppVersionAppComponent' {name} -> name) (\s@UpdateAppVersionAppComponent' {} a -> s {name = a} :: UpdateAppVersionAppComponent)

-- | The type of Application Component. For more information about the types
-- of Application Component, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
updateAppVersionAppComponent_type :: Lens.Lens' UpdateAppVersionAppComponent (Prelude.Maybe Prelude.Text)
updateAppVersionAppComponent_type = Lens.lens (\UpdateAppVersionAppComponent' {type'} -> type') (\s@UpdateAppVersionAppComponent' {} a -> s {type' = a} :: UpdateAppVersionAppComponent)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
updateAppVersionAppComponent_appArn :: Lens.Lens' UpdateAppVersionAppComponent Prelude.Text
updateAppVersionAppComponent_appArn = Lens.lens (\UpdateAppVersionAppComponent' {appArn} -> appArn) (\s@UpdateAppVersionAppComponent' {} a -> s {appArn = a} :: UpdateAppVersionAppComponent)

-- | The identifier of the Application Component.
updateAppVersionAppComponent_id :: Lens.Lens' UpdateAppVersionAppComponent Prelude.Text
updateAppVersionAppComponent_id = Lens.lens (\UpdateAppVersionAppComponent' {id} -> id) (\s@UpdateAppVersionAppComponent' {} a -> s {id = a} :: UpdateAppVersionAppComponent)

instance Core.AWSRequest UpdateAppVersionAppComponent where
  type
    AWSResponse UpdateAppVersionAppComponent =
      UpdateAppVersionAppComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppVersionAppComponentResponse'
            Prelude.<$> (x Data..?> "appComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance
  Prelude.Hashable
    UpdateAppVersionAppComponent
  where
  hashWithSalt _salt UpdateAppVersionAppComponent' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateAppVersionAppComponent where
  rnf UpdateAppVersionAppComponent' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateAppVersionAppComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAppVersionAppComponent where
  toJSON UpdateAppVersionAppComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalInfo" Data..=)
              Prelude.<$> additionalInfo,
            ("name" Data..=) Prelude.<$> name,
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateAppVersionAppComponent where
  toPath =
    Prelude.const "/update-app-version-app-component"

instance Data.ToQuery UpdateAppVersionAppComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppVersionAppComponentResponse' smart constructor.
data UpdateAppVersionAppComponentResponse = UpdateAppVersionAppComponentResponse'
  { -- | The list of Application Components that belong to this resource.
    appComponent :: Prelude.Maybe AppComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The Resilience Hub application version.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppVersionAppComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponent', 'updateAppVersionAppComponentResponse_appComponent' - The list of Application Components that belong to this resource.
--
-- 'httpStatus', 'updateAppVersionAppComponentResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'updateAppVersionAppComponentResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'updateAppVersionAppComponentResponse_appVersion' - The Resilience Hub application version.
newUpdateAppVersionAppComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  UpdateAppVersionAppComponentResponse
newUpdateAppVersionAppComponentResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    UpdateAppVersionAppComponentResponse'
      { appComponent =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | The list of Application Components that belong to this resource.
updateAppVersionAppComponentResponse_appComponent :: Lens.Lens' UpdateAppVersionAppComponentResponse (Prelude.Maybe AppComponent)
updateAppVersionAppComponentResponse_appComponent = Lens.lens (\UpdateAppVersionAppComponentResponse' {appComponent} -> appComponent) (\s@UpdateAppVersionAppComponentResponse' {} a -> s {appComponent = a} :: UpdateAppVersionAppComponentResponse)

-- | The response's http status code.
updateAppVersionAppComponentResponse_httpStatus :: Lens.Lens' UpdateAppVersionAppComponentResponse Prelude.Int
updateAppVersionAppComponentResponse_httpStatus = Lens.lens (\UpdateAppVersionAppComponentResponse' {httpStatus} -> httpStatus) (\s@UpdateAppVersionAppComponentResponse' {} a -> s {httpStatus = a} :: UpdateAppVersionAppComponentResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
updateAppVersionAppComponentResponse_appArn :: Lens.Lens' UpdateAppVersionAppComponentResponse Prelude.Text
updateAppVersionAppComponentResponse_appArn = Lens.lens (\UpdateAppVersionAppComponentResponse' {appArn} -> appArn) (\s@UpdateAppVersionAppComponentResponse' {} a -> s {appArn = a} :: UpdateAppVersionAppComponentResponse)

-- | The Resilience Hub application version.
updateAppVersionAppComponentResponse_appVersion :: Lens.Lens' UpdateAppVersionAppComponentResponse Prelude.Text
updateAppVersionAppComponentResponse_appVersion = Lens.lens (\UpdateAppVersionAppComponentResponse' {appVersion} -> appVersion) (\s@UpdateAppVersionAppComponentResponse' {} a -> s {appVersion = a} :: UpdateAppVersionAppComponentResponse)

instance
  Prelude.NFData
    UpdateAppVersionAppComponentResponse
  where
  rnf UpdateAppVersionAppComponentResponse' {..} =
    Prelude.rnf appComponent
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
