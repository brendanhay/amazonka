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
-- Module      : Amazonka.ResilienceHub.DeleteAppVersionAppComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Application Component from the Resilience Hub application.
--
-- -   This API updates the Resilience Hub application draft version. To
--     use this Application Component for running assessments, you must
--     publish the Resilience Hub application using the @PublishAppVersion@
--     API.
--
-- -   You will not be able to delete an Application Component if it has
--     resources associated with it.
module Amazonka.ResilienceHub.DeleteAppVersionAppComponent
  ( -- * Creating a Request
    DeleteAppVersionAppComponent (..),
    newDeleteAppVersionAppComponent,

    -- * Request Lenses
    deleteAppVersionAppComponent_clientToken,
    deleteAppVersionAppComponent_appArn,
    deleteAppVersionAppComponent_id,

    -- * Destructuring the Response
    DeleteAppVersionAppComponentResponse (..),
    newDeleteAppVersionAppComponentResponse,

    -- * Response Lenses
    deleteAppVersionAppComponentResponse_appComponent,
    deleteAppVersionAppComponentResponse_httpStatus,
    deleteAppVersionAppComponentResponse_appArn,
    deleteAppVersionAppComponentResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppVersionAppComponent' smart constructor.
data DeleteAppVersionAppComponent = DeleteAppVersionAppComponent'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'DeleteAppVersionAppComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAppVersionAppComponent_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'appArn', 'deleteAppVersionAppComponent_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'id', 'deleteAppVersionAppComponent_id' - The identifier of the Application Component.
newDeleteAppVersionAppComponent ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteAppVersionAppComponent
newDeleteAppVersionAppComponent pAppArn_ pId_ =
  DeleteAppVersionAppComponent'
    { clientToken =
        Prelude.Nothing,
      appArn = pAppArn_,
      id = pId_
    }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteAppVersionAppComponent_clientToken :: Lens.Lens' DeleteAppVersionAppComponent (Prelude.Maybe Prelude.Text)
deleteAppVersionAppComponent_clientToken = Lens.lens (\DeleteAppVersionAppComponent' {clientToken} -> clientToken) (\s@DeleteAppVersionAppComponent' {} a -> s {clientToken = a} :: DeleteAppVersionAppComponent)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppVersionAppComponent_appArn :: Lens.Lens' DeleteAppVersionAppComponent Prelude.Text
deleteAppVersionAppComponent_appArn = Lens.lens (\DeleteAppVersionAppComponent' {appArn} -> appArn) (\s@DeleteAppVersionAppComponent' {} a -> s {appArn = a} :: DeleteAppVersionAppComponent)

-- | The identifier of the Application Component.
deleteAppVersionAppComponent_id :: Lens.Lens' DeleteAppVersionAppComponent Prelude.Text
deleteAppVersionAppComponent_id = Lens.lens (\DeleteAppVersionAppComponent' {id} -> id) (\s@DeleteAppVersionAppComponent' {} a -> s {id = a} :: DeleteAppVersionAppComponent)

instance Core.AWSRequest DeleteAppVersionAppComponent where
  type
    AWSResponse DeleteAppVersionAppComponent =
      DeleteAppVersionAppComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAppVersionAppComponentResponse'
            Prelude.<$> (x Data..?> "appComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance
  Prelude.Hashable
    DeleteAppVersionAppComponent
  where
  hashWithSalt _salt DeleteAppVersionAppComponent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteAppVersionAppComponent where
  rnf DeleteAppVersionAppComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteAppVersionAppComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAppVersionAppComponent where
  toJSON DeleteAppVersionAppComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath DeleteAppVersionAppComponent where
  toPath =
    Prelude.const "/delete-app-version-app-component"

instance Data.ToQuery DeleteAppVersionAppComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppVersionAppComponentResponse' smart constructor.
data DeleteAppVersionAppComponentResponse = DeleteAppVersionAppComponentResponse'
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
-- Create a value of 'DeleteAppVersionAppComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponent', 'deleteAppVersionAppComponentResponse_appComponent' - The list of Application Components that belong to this resource.
--
-- 'httpStatus', 'deleteAppVersionAppComponentResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'deleteAppVersionAppComponentResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'deleteAppVersionAppComponentResponse_appVersion' - The Resilience Hub application version.
newDeleteAppVersionAppComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DeleteAppVersionAppComponentResponse
newDeleteAppVersionAppComponentResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    DeleteAppVersionAppComponentResponse'
      { appComponent =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | The list of Application Components that belong to this resource.
deleteAppVersionAppComponentResponse_appComponent :: Lens.Lens' DeleteAppVersionAppComponentResponse (Prelude.Maybe AppComponent)
deleteAppVersionAppComponentResponse_appComponent = Lens.lens (\DeleteAppVersionAppComponentResponse' {appComponent} -> appComponent) (\s@DeleteAppVersionAppComponentResponse' {} a -> s {appComponent = a} :: DeleteAppVersionAppComponentResponse)

-- | The response's http status code.
deleteAppVersionAppComponentResponse_httpStatus :: Lens.Lens' DeleteAppVersionAppComponentResponse Prelude.Int
deleteAppVersionAppComponentResponse_httpStatus = Lens.lens (\DeleteAppVersionAppComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteAppVersionAppComponentResponse' {} a -> s {httpStatus = a} :: DeleteAppVersionAppComponentResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppVersionAppComponentResponse_appArn :: Lens.Lens' DeleteAppVersionAppComponentResponse Prelude.Text
deleteAppVersionAppComponentResponse_appArn = Lens.lens (\DeleteAppVersionAppComponentResponse' {appArn} -> appArn) (\s@DeleteAppVersionAppComponentResponse' {} a -> s {appArn = a} :: DeleteAppVersionAppComponentResponse)

-- | The Resilience Hub application version.
deleteAppVersionAppComponentResponse_appVersion :: Lens.Lens' DeleteAppVersionAppComponentResponse Prelude.Text
deleteAppVersionAppComponentResponse_appVersion = Lens.lens (\DeleteAppVersionAppComponentResponse' {appVersion} -> appVersion) (\s@DeleteAppVersionAppComponentResponse' {} a -> s {appVersion = a} :: DeleteAppVersionAppComponentResponse)

instance
  Prelude.NFData
    DeleteAppVersionAppComponentResponse
  where
  rnf DeleteAppVersionAppComponentResponse' {..} =
    Prelude.rnf appComponent
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
