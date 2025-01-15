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
-- Module      : Amazonka.ResilienceHub.DeleteApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Resilience Hub application. This is a destructive action
-- that can\'t be undone.
module Amazonka.ResilienceHub.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_clientToken,
    deleteApp_forceDelete,
    deleteApp_appArn,

    -- * Destructuring the Response
    DeleteAppResponse (..),
    newDeleteAppResponse,

    -- * Response Lenses
    deleteAppResponse_httpStatus,
    deleteAppResponse_appArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A boolean option to force the deletion of a Resilience Hub application.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteApp_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'forceDelete', 'deleteApp_forceDelete' - A boolean option to force the deletion of a Resilience Hub application.
--
-- 'appArn', 'deleteApp_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDeleteApp ::
  -- | 'appArn'
  Prelude.Text ->
  DeleteApp
newDeleteApp pAppArn_ =
  DeleteApp'
    { clientToken = Prelude.Nothing,
      forceDelete = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteApp_clientToken :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Text)
deleteApp_clientToken = Lens.lens (\DeleteApp' {clientToken} -> clientToken) (\s@DeleteApp' {} a -> s {clientToken = a} :: DeleteApp)

-- | A boolean option to force the deletion of a Resilience Hub application.
deleteApp_forceDelete :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Bool)
deleteApp_forceDelete = Lens.lens (\DeleteApp' {forceDelete} -> forceDelete) (\s@DeleteApp' {} a -> s {forceDelete = a} :: DeleteApp)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
deleteApp_appArn :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_appArn = Lens.lens (\DeleteApp' {appArn} -> appArn) (\s@DeleteApp' {} a -> s {appArn = a} :: DeleteApp)

instance Core.AWSRequest DeleteApp where
  type AWSResponse DeleteApp = DeleteAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
      )

instance Prelude.Hashable DeleteApp where
  hashWithSalt _salt DeleteApp' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData DeleteApp where
  rnf DeleteApp' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf forceDelete `Prelude.seq`
        Prelude.rnf appArn

instance Data.ToHeaders DeleteApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("forceDelete" Data..=) Prelude.<$> forceDelete,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath DeleteApp where
  toPath = Prelude.const "/delete-app"

instance Data.ToQuery DeleteApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'deleteAppResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDeleteAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  DeleteAppResponse
newDeleteAppResponse pHttpStatus_ pAppArn_ =
  DeleteAppResponse'
    { httpStatus = pHttpStatus_,
      appArn = pAppArn_
    }

-- | The response's http status code.
deleteAppResponse_httpStatus :: Lens.Lens' DeleteAppResponse Prelude.Int
deleteAppResponse_httpStatus = Lens.lens (\DeleteAppResponse' {httpStatus} -> httpStatus) (\s@DeleteAppResponse' {} a -> s {httpStatus = a} :: DeleteAppResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
deleteAppResponse_appArn :: Lens.Lens' DeleteAppResponse Prelude.Text
deleteAppResponse_appArn = Lens.lens (\DeleteAppResponse' {appArn} -> appArn) (\s@DeleteAppResponse' {} a -> s {appArn = a} :: DeleteAppResponse)

instance Prelude.NFData DeleteAppResponse where
  rnf DeleteAppResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf appArn
