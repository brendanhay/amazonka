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
-- Module      : Amazonka.ECS.DeleteAccountSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an account setting for a specified user, role, or the root user
-- for an account.
module Amazonka.ECS.DeleteAccountSetting
  ( -- * Creating a Request
    DeleteAccountSetting (..),
    newDeleteAccountSetting,

    -- * Request Lenses
    deleteAccountSetting_principalArn,
    deleteAccountSetting_name,

    -- * Destructuring the Response
    DeleteAccountSettingResponse (..),
    newDeleteAccountSettingResponse,

    -- * Response Lenses
    deleteAccountSettingResponse_setting,
    deleteAccountSettingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountSetting' smart constructor.
data DeleteAccountSetting = DeleteAccountSetting'
  { -- | The Amazon Resource Name (ARN) of the principal. It can be an user,
    -- role, or the root user. If you specify the root user, it disables the
    -- account setting for all users, roles, and the root user of the account
    -- unless a user or role explicitly overrides these settings. If this field
    -- is omitted, the setting is changed only for the authenticated user.
    principalArn :: Prelude.Maybe Prelude.Text,
    -- | The resource name to disable the account setting for. If
    -- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
    -- services is affected. If @taskLongArnFormat@ is specified, the ARN and
    -- resource ID for your Amazon ECS tasks is affected. If
    -- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
    -- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
    -- is specified, the ENI limit for your Amazon ECS container instances is
    -- affected.
    name :: SettingName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalArn', 'deleteAccountSetting_principalArn' - The Amazon Resource Name (ARN) of the principal. It can be an user,
-- role, or the root user. If you specify the root user, it disables the
-- account setting for all users, roles, and the root user of the account
-- unless a user or role explicitly overrides these settings. If this field
-- is omitted, the setting is changed only for the authenticated user.
--
-- 'name', 'deleteAccountSetting_name' - The resource name to disable the account setting for. If
-- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
-- services is affected. If @taskLongArnFormat@ is specified, the ARN and
-- resource ID for your Amazon ECS tasks is affected. If
-- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
-- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
-- is specified, the ENI limit for your Amazon ECS container instances is
-- affected.
newDeleteAccountSetting ::
  -- | 'name'
  SettingName ->
  DeleteAccountSetting
newDeleteAccountSetting pName_ =
  DeleteAccountSetting'
    { principalArn =
        Prelude.Nothing,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the principal. It can be an user,
-- role, or the root user. If you specify the root user, it disables the
-- account setting for all users, roles, and the root user of the account
-- unless a user or role explicitly overrides these settings. If this field
-- is omitted, the setting is changed only for the authenticated user.
deleteAccountSetting_principalArn :: Lens.Lens' DeleteAccountSetting (Prelude.Maybe Prelude.Text)
deleteAccountSetting_principalArn = Lens.lens (\DeleteAccountSetting' {principalArn} -> principalArn) (\s@DeleteAccountSetting' {} a -> s {principalArn = a} :: DeleteAccountSetting)

-- | The resource name to disable the account setting for. If
-- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
-- services is affected. If @taskLongArnFormat@ is specified, the ARN and
-- resource ID for your Amazon ECS tasks is affected. If
-- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
-- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
-- is specified, the ENI limit for your Amazon ECS container instances is
-- affected.
deleteAccountSetting_name :: Lens.Lens' DeleteAccountSetting SettingName
deleteAccountSetting_name = Lens.lens (\DeleteAccountSetting' {name} -> name) (\s@DeleteAccountSetting' {} a -> s {name = a} :: DeleteAccountSetting)

instance Core.AWSRequest DeleteAccountSetting where
  type
    AWSResponse DeleteAccountSetting =
      DeleteAccountSettingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAccountSettingResponse'
            Prelude.<$> (x Data..?> "setting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccountSetting where
  hashWithSalt _salt DeleteAccountSetting' {..} =
    _salt
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteAccountSetting where
  rnf DeleteAccountSetting' {..} =
    Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteAccountSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAccountSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccountSetting where
  toJSON DeleteAccountSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("principalArn" Data..=) Prelude.<$> principalArn,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath DeleteAccountSetting where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccountSetting where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccountSettingResponse' smart constructor.
data DeleteAccountSettingResponse = DeleteAccountSettingResponse'
  { -- | The account setting for the specified principal ARN.
    setting :: Prelude.Maybe Setting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountSettingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setting', 'deleteAccountSettingResponse_setting' - The account setting for the specified principal ARN.
--
-- 'httpStatus', 'deleteAccountSettingResponse_httpStatus' - The response's http status code.
newDeleteAccountSettingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccountSettingResponse
newDeleteAccountSettingResponse pHttpStatus_ =
  DeleteAccountSettingResponse'
    { setting =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The account setting for the specified principal ARN.
deleteAccountSettingResponse_setting :: Lens.Lens' DeleteAccountSettingResponse (Prelude.Maybe Setting)
deleteAccountSettingResponse_setting = Lens.lens (\DeleteAccountSettingResponse' {setting} -> setting) (\s@DeleteAccountSettingResponse' {} a -> s {setting = a} :: DeleteAccountSettingResponse)

-- | The response's http status code.
deleteAccountSettingResponse_httpStatus :: Lens.Lens' DeleteAccountSettingResponse Prelude.Int
deleteAccountSettingResponse_httpStatus = Lens.lens (\DeleteAccountSettingResponse' {httpStatus} -> httpStatus) (\s@DeleteAccountSettingResponse' {} a -> s {httpStatus = a} :: DeleteAccountSettingResponse)

instance Prelude.NFData DeleteAccountSettingResponse where
  rnf DeleteAccountSettingResponse' {..} =
    Prelude.rnf setting
      `Prelude.seq` Prelude.rnf httpStatus
