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
-- Module      : Amazonka.ECS.PutAccountSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an account setting. Account settings are set on a per-Region
-- basis.
--
-- If you change the account setting for the root user, the default
-- settings for all of the IAM users and roles that no individual account
-- setting was specified are reset for. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html Account Settings>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- When @serviceLongArnFormat@, @taskLongArnFormat@, or
-- @containerInstanceLongArnFormat@ are specified, the Amazon Resource Name
-- (ARN) and resource ID format of the resource type for a specified IAM
-- user, IAM role, or the root user for an account is affected. The opt-in
-- and opt-out account setting must be set for each Amazon ECS resource
-- separately. The ARN and resource ID format of a resource is defined by
-- the opt-in status of the IAM user or role that created the resource. You
-- must turn on this setting to use Amazon ECS features such as resource
-- tagging.
--
-- When @awsvpcTrunking@ is specified, the elastic network interface (ENI)
-- limit for any new container instances that support the feature is
-- changed. If @awsvpcTrunking@ is enabled, any new container instances
-- that support the feature are launched have the increased ENI limits
-- available to them. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-eni.html Elastic Network Interface Trunking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- When @containerInsights@ is specified, the default setting indicating
-- whether CloudWatch Container Insights is enabled for your clusters is
-- changed. If @containerInsights@ is enabled, any new clusters that are
-- created will have Container Insights enabled unless you disable it
-- during cluster creation. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cloudwatch-container-insights.html CloudWatch Container Insights>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.PutAccountSetting
  ( -- * Creating a Request
    PutAccountSetting (..),
    newPutAccountSetting,

    -- * Request Lenses
    putAccountSetting_principalArn,
    putAccountSetting_name,
    putAccountSetting_value,

    -- * Destructuring the Response
    PutAccountSettingResponse (..),
    newPutAccountSettingResponse,

    -- * Response Lenses
    putAccountSettingResponse_setting,
    putAccountSettingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAccountSetting' smart constructor.
data PutAccountSetting = PutAccountSetting'
  { -- | The ARN of the principal, which can be an IAM user, IAM role, or the
    -- root user. If you specify the root user, it modifies the account setting
    -- for all IAM users, IAM roles, and the root user of the account unless an
    -- IAM user or role explicitly overrides these settings. If this field is
    -- omitted, the setting is changed only for the authenticated user.
    --
    -- Federated users assume the account setting of the root user and can\'t
    -- have explicit account settings set for them.
    principalArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon ECS resource name for which to modify the account setting. If
    -- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
    -- services is affected. If @taskLongArnFormat@ is specified, the ARN and
    -- resource ID for your Amazon ECS tasks is affected. If
    -- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
    -- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
    -- is specified, the elastic network interface (ENI) limit for your Amazon
    -- ECS container instances is affected. If @containerInsights@ is
    -- specified, the default setting for CloudWatch Container Insights for
    -- your clusters is affected.
    name :: SettingName,
    -- | The account setting value for the specified principal ARN. Accepted
    -- values are @enabled@ and @disabled@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalArn', 'putAccountSetting_principalArn' - The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If you specify the root user, it modifies the account setting
-- for all IAM users, IAM roles, and the root user of the account unless an
-- IAM user or role explicitly overrides these settings. If this field is
-- omitted, the setting is changed only for the authenticated user.
--
-- Federated users assume the account setting of the root user and can\'t
-- have explicit account settings set for them.
--
-- 'name', 'putAccountSetting_name' - The Amazon ECS resource name for which to modify the account setting. If
-- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
-- services is affected. If @taskLongArnFormat@ is specified, the ARN and
-- resource ID for your Amazon ECS tasks is affected. If
-- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
-- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
-- is specified, the elastic network interface (ENI) limit for your Amazon
-- ECS container instances is affected. If @containerInsights@ is
-- specified, the default setting for CloudWatch Container Insights for
-- your clusters is affected.
--
-- 'value', 'putAccountSetting_value' - The account setting value for the specified principal ARN. Accepted
-- values are @enabled@ and @disabled@.
newPutAccountSetting ::
  -- | 'name'
  SettingName ->
  -- | 'value'
  Prelude.Text ->
  PutAccountSetting
newPutAccountSetting pName_ pValue_ =
  PutAccountSetting'
    { principalArn = Prelude.Nothing,
      name = pName_,
      value = pValue_
    }

-- | The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If you specify the root user, it modifies the account setting
-- for all IAM users, IAM roles, and the root user of the account unless an
-- IAM user or role explicitly overrides these settings. If this field is
-- omitted, the setting is changed only for the authenticated user.
--
-- Federated users assume the account setting of the root user and can\'t
-- have explicit account settings set for them.
putAccountSetting_principalArn :: Lens.Lens' PutAccountSetting (Prelude.Maybe Prelude.Text)
putAccountSetting_principalArn = Lens.lens (\PutAccountSetting' {principalArn} -> principalArn) (\s@PutAccountSetting' {} a -> s {principalArn = a} :: PutAccountSetting)

-- | The Amazon ECS resource name for which to modify the account setting. If
-- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
-- services is affected. If @taskLongArnFormat@ is specified, the ARN and
-- resource ID for your Amazon ECS tasks is affected. If
-- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
-- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
-- is specified, the elastic network interface (ENI) limit for your Amazon
-- ECS container instances is affected. If @containerInsights@ is
-- specified, the default setting for CloudWatch Container Insights for
-- your clusters is affected.
putAccountSetting_name :: Lens.Lens' PutAccountSetting SettingName
putAccountSetting_name = Lens.lens (\PutAccountSetting' {name} -> name) (\s@PutAccountSetting' {} a -> s {name = a} :: PutAccountSetting)

-- | The account setting value for the specified principal ARN. Accepted
-- values are @enabled@ and @disabled@.
putAccountSetting_value :: Lens.Lens' PutAccountSetting Prelude.Text
putAccountSetting_value = Lens.lens (\PutAccountSetting' {value} -> value) (\s@PutAccountSetting' {} a -> s {value = a} :: PutAccountSetting)

instance Core.AWSRequest PutAccountSetting where
  type
    AWSResponse PutAccountSetting =
      PutAccountSettingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountSettingResponse'
            Prelude.<$> (x Data..?> "setting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountSetting where
  hashWithSalt _salt PutAccountSetting' {..} =
    _salt
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData PutAccountSetting where
  rnf PutAccountSetting' {..} =
    Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToHeaders PutAccountSetting where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.PutAccountSetting" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountSetting where
  toJSON PutAccountSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("principalArn" Data..=) Prelude.<$> principalArn,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )

instance Data.ToPath PutAccountSetting where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAccountSetting where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountSettingResponse' smart constructor.
data PutAccountSettingResponse = PutAccountSettingResponse'
  { -- | The current account setting for a resource.
    setting :: Prelude.Maybe Setting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSettingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setting', 'putAccountSettingResponse_setting' - The current account setting for a resource.
--
-- 'httpStatus', 'putAccountSettingResponse_httpStatus' - The response's http status code.
newPutAccountSettingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountSettingResponse
newPutAccountSettingResponse pHttpStatus_ =
  PutAccountSettingResponse'
    { setting =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current account setting for a resource.
putAccountSettingResponse_setting :: Lens.Lens' PutAccountSettingResponse (Prelude.Maybe Setting)
putAccountSettingResponse_setting = Lens.lens (\PutAccountSettingResponse' {setting} -> setting) (\s@PutAccountSettingResponse' {} a -> s {setting = a} :: PutAccountSettingResponse)

-- | The response's http status code.
putAccountSettingResponse_httpStatus :: Lens.Lens' PutAccountSettingResponse Prelude.Int
putAccountSettingResponse_httpStatus = Lens.lens (\PutAccountSettingResponse' {httpStatus} -> httpStatus) (\s@PutAccountSettingResponse' {} a -> s {httpStatus = a} :: PutAccountSettingResponse)

instance Prelude.NFData PutAccountSettingResponse where
  rnf PutAccountSettingResponse' {..} =
    Prelude.rnf setting
      `Prelude.seq` Prelude.rnf httpStatus
