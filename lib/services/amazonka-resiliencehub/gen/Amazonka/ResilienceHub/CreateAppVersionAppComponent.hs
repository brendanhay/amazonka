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
-- Module      : Amazonka.ResilienceHub.CreateAppVersionAppComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Application Component in the Resilience Hub application.
--
-- This API updates the Resilience Hub application draft version. To use
-- this Application Component for running assessments, you must publish the
-- Resilience Hub application using the @PublishAppVersion@ API.
module Amazonka.ResilienceHub.CreateAppVersionAppComponent
  ( -- * Creating a Request
    CreateAppVersionAppComponent (..),
    newCreateAppVersionAppComponent,

    -- * Request Lenses
    createAppVersionAppComponent_additionalInfo,
    createAppVersionAppComponent_clientToken,
    createAppVersionAppComponent_id,
    createAppVersionAppComponent_appArn,
    createAppVersionAppComponent_name,
    createAppVersionAppComponent_type,

    -- * Destructuring the Response
    CreateAppVersionAppComponentResponse (..),
    newCreateAppVersionAppComponentResponse,

    -- * Response Lenses
    createAppVersionAppComponentResponse_appComponent,
    createAppVersionAppComponentResponse_httpStatus,
    createAppVersionAppComponentResponse_appArn,
    createAppVersionAppComponentResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAppVersionAppComponent' smart constructor.
data CreateAppVersionAppComponent = CreateAppVersionAppComponent'
  { -- | Currently, there is no supported additional information for Application
    -- Components.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Application Component.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The name of the Application Component.
    name :: Prelude.Text,
    -- | The type of Application Component. For more information about the types
    -- of Application Component, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppVersionAppComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'createAppVersionAppComponent_additionalInfo' - Currently, there is no supported additional information for Application
-- Components.
--
-- 'clientToken', 'createAppVersionAppComponent_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'id', 'createAppVersionAppComponent_id' - The identifier of the Application Component.
--
-- 'appArn', 'createAppVersionAppComponent_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'name', 'createAppVersionAppComponent_name' - The name of the Application Component.
--
-- 'type'', 'createAppVersionAppComponent_type' - The type of Application Component. For more information about the types
-- of Application Component, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
newCreateAppVersionAppComponent ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  CreateAppVersionAppComponent
newCreateAppVersionAppComponent
  pAppArn_
  pName_
  pType_ =
    CreateAppVersionAppComponent'
      { additionalInfo =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        id = Prelude.Nothing,
        appArn = pAppArn_,
        name = pName_,
        type' = pType_
      }

-- | Currently, there is no supported additional information for Application
-- Components.
createAppVersionAppComponent_additionalInfo :: Lens.Lens' CreateAppVersionAppComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
createAppVersionAppComponent_additionalInfo = Lens.lens (\CreateAppVersionAppComponent' {additionalInfo} -> additionalInfo) (\s@CreateAppVersionAppComponent' {} a -> s {additionalInfo = a} :: CreateAppVersionAppComponent) Prelude.. Lens.mapping Lens.coerced

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
createAppVersionAppComponent_clientToken :: Lens.Lens' CreateAppVersionAppComponent (Prelude.Maybe Prelude.Text)
createAppVersionAppComponent_clientToken = Lens.lens (\CreateAppVersionAppComponent' {clientToken} -> clientToken) (\s@CreateAppVersionAppComponent' {} a -> s {clientToken = a} :: CreateAppVersionAppComponent)

-- | The identifier of the Application Component.
createAppVersionAppComponent_id :: Lens.Lens' CreateAppVersionAppComponent (Prelude.Maybe Prelude.Text)
createAppVersionAppComponent_id = Lens.lens (\CreateAppVersionAppComponent' {id} -> id) (\s@CreateAppVersionAppComponent' {} a -> s {id = a} :: CreateAppVersionAppComponent)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
createAppVersionAppComponent_appArn :: Lens.Lens' CreateAppVersionAppComponent Prelude.Text
createAppVersionAppComponent_appArn = Lens.lens (\CreateAppVersionAppComponent' {appArn} -> appArn) (\s@CreateAppVersionAppComponent' {} a -> s {appArn = a} :: CreateAppVersionAppComponent)

-- | The name of the Application Component.
createAppVersionAppComponent_name :: Lens.Lens' CreateAppVersionAppComponent Prelude.Text
createAppVersionAppComponent_name = Lens.lens (\CreateAppVersionAppComponent' {name} -> name) (\s@CreateAppVersionAppComponent' {} a -> s {name = a} :: CreateAppVersionAppComponent)

-- | The type of Application Component. For more information about the types
-- of Application Component, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
createAppVersionAppComponent_type :: Lens.Lens' CreateAppVersionAppComponent Prelude.Text
createAppVersionAppComponent_type = Lens.lens (\CreateAppVersionAppComponent' {type'} -> type') (\s@CreateAppVersionAppComponent' {} a -> s {type' = a} :: CreateAppVersionAppComponent)

instance Core.AWSRequest CreateAppVersionAppComponent where
  type
    AWSResponse CreateAppVersionAppComponent =
      CreateAppVersionAppComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppVersionAppComponentResponse'
            Prelude.<$> (x Data..?> "appComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance
  Prelude.Hashable
    CreateAppVersionAppComponent
  where
  hashWithSalt _salt CreateAppVersionAppComponent' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateAppVersionAppComponent where
  rnf CreateAppVersionAppComponent' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateAppVersionAppComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAppVersionAppComponent where
  toJSON CreateAppVersionAppComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalInfo" Data..=)
              Prelude.<$> additionalInfo,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("id" Data..=) Prelude.<$> id,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateAppVersionAppComponent where
  toPath =
    Prelude.const "/create-app-version-app-component"

instance Data.ToQuery CreateAppVersionAppComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppVersionAppComponentResponse' smart constructor.
data CreateAppVersionAppComponentResponse = CreateAppVersionAppComponentResponse'
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
-- Create a value of 'CreateAppVersionAppComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponent', 'createAppVersionAppComponentResponse_appComponent' - The list of Application Components that belong to this resource.
--
-- 'httpStatus', 'createAppVersionAppComponentResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'createAppVersionAppComponentResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'createAppVersionAppComponentResponse_appVersion' - The Resilience Hub application version.
newCreateAppVersionAppComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  CreateAppVersionAppComponentResponse
newCreateAppVersionAppComponentResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    CreateAppVersionAppComponentResponse'
      { appComponent =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | The list of Application Components that belong to this resource.
createAppVersionAppComponentResponse_appComponent :: Lens.Lens' CreateAppVersionAppComponentResponse (Prelude.Maybe AppComponent)
createAppVersionAppComponentResponse_appComponent = Lens.lens (\CreateAppVersionAppComponentResponse' {appComponent} -> appComponent) (\s@CreateAppVersionAppComponentResponse' {} a -> s {appComponent = a} :: CreateAppVersionAppComponentResponse)

-- | The response's http status code.
createAppVersionAppComponentResponse_httpStatus :: Lens.Lens' CreateAppVersionAppComponentResponse Prelude.Int
createAppVersionAppComponentResponse_httpStatus = Lens.lens (\CreateAppVersionAppComponentResponse' {httpStatus} -> httpStatus) (\s@CreateAppVersionAppComponentResponse' {} a -> s {httpStatus = a} :: CreateAppVersionAppComponentResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
createAppVersionAppComponentResponse_appArn :: Lens.Lens' CreateAppVersionAppComponentResponse Prelude.Text
createAppVersionAppComponentResponse_appArn = Lens.lens (\CreateAppVersionAppComponentResponse' {appArn} -> appArn) (\s@CreateAppVersionAppComponentResponse' {} a -> s {appArn = a} :: CreateAppVersionAppComponentResponse)

-- | The Resilience Hub application version.
createAppVersionAppComponentResponse_appVersion :: Lens.Lens' CreateAppVersionAppComponentResponse Prelude.Text
createAppVersionAppComponentResponse_appVersion = Lens.lens (\CreateAppVersionAppComponentResponse' {appVersion} -> appVersion) (\s@CreateAppVersionAppComponentResponse' {} a -> s {appVersion = a} :: CreateAppVersionAppComponentResponse)

instance
  Prelude.NFData
    CreateAppVersionAppComponentResponse
  where
  rnf CreateAppVersionAppComponentResponse' {..} =
    Prelude.rnf appComponent
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
