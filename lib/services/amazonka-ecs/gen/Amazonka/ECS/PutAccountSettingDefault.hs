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
-- Module      : Amazonka.ECS.PutAccountSettingDefault
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an account setting for all users on an account for whom no
-- individual account setting has been specified. Account settings are set
-- on a per-Region basis.
module Amazonka.ECS.PutAccountSettingDefault
  ( -- * Creating a Request
    PutAccountSettingDefault (..),
    newPutAccountSettingDefault,

    -- * Request Lenses
    putAccountSettingDefault_name,
    putAccountSettingDefault_value,

    -- * Destructuring the Response
    PutAccountSettingDefaultResponse (..),
    newPutAccountSettingDefaultResponse,

    -- * Response Lenses
    putAccountSettingDefaultResponse_setting,
    putAccountSettingDefaultResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAccountSettingDefault' smart constructor.
data PutAccountSettingDefault = PutAccountSettingDefault'
  { -- | The resource name for which to modify the account setting. If
    -- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
    -- services is affected. If @taskLongArnFormat@ is specified, the ARN and
    -- resource ID for your Amazon ECS tasks is affected. If
    -- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
    -- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
    -- is specified, the ENI limit for your Amazon ECS container instances is
    -- affected. If @containerInsights@ is specified, the default setting for
    -- Amazon Web Services CloudWatch Container Insights for your clusters is
    -- affected. If @tagResourceAuthorization@ is specified, the opt-in option
    -- for tagging resources on creation is affected. For information about the
    -- opt-in timeline, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#tag-resources Tagging authorization timeline>
    -- in the /Amazon ECS Developer Guide/.
    --
    -- When you specify @fargateFIPSMode@ for the @name@ and @enabled@ for the
    -- @value@, Fargate uses FIPS-140 compliant cryptographic algorithms on
    -- your tasks. For more information about FIPS-140 compliance with Fargate,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-fips-compliance.html Amazon Web Services Fargate Federal Information Processing Standard (FIPS) 140-2 compliance>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    name :: SettingName,
    -- | The account setting value for the specified principal ARN. Accepted
    -- values are @enabled@, @disabled@, @on@, and @off@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSettingDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'putAccountSettingDefault_name' - The resource name for which to modify the account setting. If
-- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
-- services is affected. If @taskLongArnFormat@ is specified, the ARN and
-- resource ID for your Amazon ECS tasks is affected. If
-- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
-- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
-- is specified, the ENI limit for your Amazon ECS container instances is
-- affected. If @containerInsights@ is specified, the default setting for
-- Amazon Web Services CloudWatch Container Insights for your clusters is
-- affected. If @tagResourceAuthorization@ is specified, the opt-in option
-- for tagging resources on creation is affected. For information about the
-- opt-in timeline, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#tag-resources Tagging authorization timeline>
-- in the /Amazon ECS Developer Guide/.
--
-- When you specify @fargateFIPSMode@ for the @name@ and @enabled@ for the
-- @value@, Fargate uses FIPS-140 compliant cryptographic algorithms on
-- your tasks. For more information about FIPS-140 compliance with Fargate,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-fips-compliance.html Amazon Web Services Fargate Federal Information Processing Standard (FIPS) 140-2 compliance>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'value', 'putAccountSettingDefault_value' - The account setting value for the specified principal ARN. Accepted
-- values are @enabled@, @disabled@, @on@, and @off@.
newPutAccountSettingDefault ::
  -- | 'name'
  SettingName ->
  -- | 'value'
  Prelude.Text ->
  PutAccountSettingDefault
newPutAccountSettingDefault pName_ pValue_ =
  PutAccountSettingDefault'
    { name = pName_,
      value = pValue_
    }

-- | The resource name for which to modify the account setting. If
-- @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS
-- services is affected. If @taskLongArnFormat@ is specified, the ARN and
-- resource ID for your Amazon ECS tasks is affected. If
-- @containerInstanceLongArnFormat@ is specified, the ARN and resource ID
-- for your Amazon ECS container instances is affected. If @awsvpcTrunking@
-- is specified, the ENI limit for your Amazon ECS container instances is
-- affected. If @containerInsights@ is specified, the default setting for
-- Amazon Web Services CloudWatch Container Insights for your clusters is
-- affected. If @tagResourceAuthorization@ is specified, the opt-in option
-- for tagging resources on creation is affected. For information about the
-- opt-in timeline, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#tag-resources Tagging authorization timeline>
-- in the /Amazon ECS Developer Guide/.
--
-- When you specify @fargateFIPSMode@ for the @name@ and @enabled@ for the
-- @value@, Fargate uses FIPS-140 compliant cryptographic algorithms on
-- your tasks. For more information about FIPS-140 compliance with Fargate,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-fips-compliance.html Amazon Web Services Fargate Federal Information Processing Standard (FIPS) 140-2 compliance>
-- in the /Amazon Elastic Container Service Developer Guide/.
putAccountSettingDefault_name :: Lens.Lens' PutAccountSettingDefault SettingName
putAccountSettingDefault_name = Lens.lens (\PutAccountSettingDefault' {name} -> name) (\s@PutAccountSettingDefault' {} a -> s {name = a} :: PutAccountSettingDefault)

-- | The account setting value for the specified principal ARN. Accepted
-- values are @enabled@, @disabled@, @on@, and @off@.
putAccountSettingDefault_value :: Lens.Lens' PutAccountSettingDefault Prelude.Text
putAccountSettingDefault_value = Lens.lens (\PutAccountSettingDefault' {value} -> value) (\s@PutAccountSettingDefault' {} a -> s {value = a} :: PutAccountSettingDefault)

instance Core.AWSRequest PutAccountSettingDefault where
  type
    AWSResponse PutAccountSettingDefault =
      PutAccountSettingDefaultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountSettingDefaultResponse'
            Prelude.<$> (x Data..?> "setting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountSettingDefault where
  hashWithSalt _salt PutAccountSettingDefault' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData PutAccountSettingDefault where
  rnf PutAccountSettingDefault' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToHeaders PutAccountSettingDefault where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.PutAccountSettingDefault" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountSettingDefault where
  toJSON PutAccountSettingDefault' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )

instance Data.ToPath PutAccountSettingDefault where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAccountSettingDefault where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountSettingDefaultResponse' smart constructor.
data PutAccountSettingDefaultResponse = PutAccountSettingDefaultResponse'
  { -- | The current setting for a resource.
    setting :: Prelude.Maybe Setting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSettingDefaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setting', 'putAccountSettingDefaultResponse_setting' - The current setting for a resource.
--
-- 'httpStatus', 'putAccountSettingDefaultResponse_httpStatus' - The response's http status code.
newPutAccountSettingDefaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountSettingDefaultResponse
newPutAccountSettingDefaultResponse pHttpStatus_ =
  PutAccountSettingDefaultResponse'
    { setting =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current setting for a resource.
putAccountSettingDefaultResponse_setting :: Lens.Lens' PutAccountSettingDefaultResponse (Prelude.Maybe Setting)
putAccountSettingDefaultResponse_setting = Lens.lens (\PutAccountSettingDefaultResponse' {setting} -> setting) (\s@PutAccountSettingDefaultResponse' {} a -> s {setting = a} :: PutAccountSettingDefaultResponse)

-- | The response's http status code.
putAccountSettingDefaultResponse_httpStatus :: Lens.Lens' PutAccountSettingDefaultResponse Prelude.Int
putAccountSettingDefaultResponse_httpStatus = Lens.lens (\PutAccountSettingDefaultResponse' {httpStatus} -> httpStatus) (\s@PutAccountSettingDefaultResponse' {} a -> s {httpStatus = a} :: PutAccountSettingDefaultResponse)

instance
  Prelude.NFData
    PutAccountSettingDefaultResponse
  where
  rnf PutAccountSettingDefaultResponse' {..} =
    Prelude.rnf setting
      `Prelude.seq` Prelude.rnf httpStatus
