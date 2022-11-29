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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an account setting for a specified IAM user, IAM role, or the
-- root user for an account.
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
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountSetting' smart constructor.
data DeleteAccountSetting = DeleteAccountSetting'
  { -- | The Amazon Resource Name (ARN) of the principal. It can be an IAM user,
    -- IAM role, or the root user. If you specify the root user, it disables
    -- the account setting for all IAM users, IAM roles, and the root user of
    -- the account unless an IAM user or role explicitly overrides these
    -- settings. If this field is omitted, the setting is changed only for the
    -- authenticated user.
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
-- 'principalArn', 'deleteAccountSetting_principalArn' - The Amazon Resource Name (ARN) of the principal. It can be an IAM user,
-- IAM role, or the root user. If you specify the root user, it disables
-- the account setting for all IAM users, IAM roles, and the root user of
-- the account unless an IAM user or role explicitly overrides these
-- settings. If this field is omitted, the setting is changed only for the
-- authenticated user.
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

-- | The Amazon Resource Name (ARN) of the principal. It can be an IAM user,
-- IAM role, or the root user. If you specify the root user, it disables
-- the account setting for all IAM users, IAM roles, and the root user of
-- the account unless an IAM user or role explicitly overrides these
-- settings. If this field is omitted, the setting is changed only for the
-- authenticated user.
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
            Prelude.<$> (x Core..?> "setting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccountSetting where
  hashWithSalt _salt DeleteAccountSetting' {..} =
    _salt `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteAccountSetting where
  rnf DeleteAccountSetting' {..} =
    Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders DeleteAccountSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAccountSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAccountSetting where
  toJSON DeleteAccountSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("principalArn" Core..=) Prelude.<$> principalArn,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath DeleteAccountSetting where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAccountSetting where
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
