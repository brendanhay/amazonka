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
-- Module      : Network.AWS.ECS.DeleteAccountSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an account setting for a specified IAM user, IAM role, or the
-- root user for an account.
module Network.AWS.ECS.DeleteAccountSetting
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

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccountSetting' smart constructor.
data DeleteAccountSetting = DeleteAccountSetting'
  { -- | The ARN of the principal, which can be an IAM user, IAM role, or the
    -- root user. If you specify the root user, it disables the account setting
    -- for all IAM users, IAM roles, and the root user of the account unless an
    -- IAM user or role explicitly overrides these settings. If this field is
    -- omitted, the setting is changed only for the authenticated user.
    principalArn :: Core.Maybe Core.Text,
    -- | The resource name for which to disable the account setting. If
    -- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
    -- services is affected. If @taskLongArnFormat@ is specified, the ARN and
    -- resource ID for your Amazon ECS tasks is affected. If
    -- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
    -- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
    -- is specified, the ENI limit for your Amazon ECS container instances is
    -- affected.
    name :: SettingName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAccountSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalArn', 'deleteAccountSetting_principalArn' - The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If you specify the root user, it disables the account setting
-- for all IAM users, IAM roles, and the root user of the account unless an
-- IAM user or role explicitly overrides these settings. If this field is
-- omitted, the setting is changed only for the authenticated user.
--
-- 'name', 'deleteAccountSetting_name' - The resource name for which to disable the account setting. If
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
    { principalArn = Core.Nothing,
      name = pName_
    }

-- | The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If you specify the root user, it disables the account setting
-- for all IAM users, IAM roles, and the root user of the account unless an
-- IAM user or role explicitly overrides these settings. If this field is
-- omitted, the setting is changed only for the authenticated user.
deleteAccountSetting_principalArn :: Lens.Lens' DeleteAccountSetting (Core.Maybe Core.Text)
deleteAccountSetting_principalArn = Lens.lens (\DeleteAccountSetting' {principalArn} -> principalArn) (\s@DeleteAccountSetting' {} a -> s {principalArn = a} :: DeleteAccountSetting)

-- | The resource name for which to disable the account setting. If
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAccountSettingResponse'
            Core.<$> (x Core..?> "setting")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAccountSetting

instance Core.NFData DeleteAccountSetting

instance Core.ToHeaders DeleteAccountSetting where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAccountSetting" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAccountSetting where
  toJSON DeleteAccountSetting' {..} =
    Core.object
      ( Core.catMaybes
          [ ("principalArn" Core..=) Core.<$> principalArn,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath DeleteAccountSetting where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAccountSetting where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAccountSettingResponse' smart constructor.
data DeleteAccountSettingResponse = DeleteAccountSettingResponse'
  { -- | The account setting for the specified principal ARN.
    setting :: Core.Maybe Setting,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteAccountSettingResponse
newDeleteAccountSettingResponse pHttpStatus_ =
  DeleteAccountSettingResponse'
    { setting =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The account setting for the specified principal ARN.
deleteAccountSettingResponse_setting :: Lens.Lens' DeleteAccountSettingResponse (Core.Maybe Setting)
deleteAccountSettingResponse_setting = Lens.lens (\DeleteAccountSettingResponse' {setting} -> setting) (\s@DeleteAccountSettingResponse' {} a -> s {setting = a} :: DeleteAccountSettingResponse)

-- | The response's http status code.
deleteAccountSettingResponse_httpStatus :: Lens.Lens' DeleteAccountSettingResponse Core.Int
deleteAccountSettingResponse_httpStatus = Lens.lens (\DeleteAccountSettingResponse' {httpStatus} -> httpStatus) (\s@DeleteAccountSettingResponse' {} a -> s {httpStatus = a} :: DeleteAccountSettingResponse)

instance Core.NFData DeleteAccountSettingResponse
