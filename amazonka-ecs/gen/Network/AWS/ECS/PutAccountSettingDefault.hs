{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.PutAccountSettingDefault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an account setting for all IAM users on an account for whom no
-- individual account setting has been specified. Account settings are set
-- on a per-Region basis.
module Network.AWS.ECS.PutAccountSettingDefault
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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- CloudWatch Container Insights for your clusters is affected.
    name :: SettingName,
    -- | The account setting value for the specified principal ARN. Accepted
    -- values are @enabled@ and @disabled@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- CloudWatch Container Insights for your clusters is affected.
--
-- 'value', 'putAccountSettingDefault_value' - The account setting value for the specified principal ARN. Accepted
-- values are @enabled@ and @disabled@.
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
-- CloudWatch Container Insights for your clusters is affected.
putAccountSettingDefault_name :: Lens.Lens' PutAccountSettingDefault SettingName
putAccountSettingDefault_name = Lens.lens (\PutAccountSettingDefault' {name} -> name) (\s@PutAccountSettingDefault' {} a -> s {name = a} :: PutAccountSettingDefault)

-- | The account setting value for the specified principal ARN. Accepted
-- values are @enabled@ and @disabled@.
putAccountSettingDefault_value :: Lens.Lens' PutAccountSettingDefault Prelude.Text
putAccountSettingDefault_value = Lens.lens (\PutAccountSettingDefault' {value} -> value) (\s@PutAccountSettingDefault' {} a -> s {value = a} :: PutAccountSettingDefault)

instance Prelude.AWSRequest PutAccountSettingDefault where
  type
    Rs PutAccountSettingDefault =
      PutAccountSettingDefaultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountSettingDefaultResponse'
            Prelude.<$> (x Prelude..?> "setting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountSettingDefault

instance Prelude.NFData PutAccountSettingDefault

instance Prelude.ToHeaders PutAccountSettingDefault where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.PutAccountSettingDefault" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutAccountSettingDefault where
  toJSON PutAccountSettingDefault' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("value" Prelude..= value)
          ]
      )

instance Prelude.ToPath PutAccountSettingDefault where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutAccountSettingDefault where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountSettingDefaultResponse' smart constructor.
data PutAccountSettingDefaultResponse = PutAccountSettingDefaultResponse'
  { setting :: Prelude.Maybe Setting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSettingDefaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setting', 'putAccountSettingDefaultResponse_setting' - Undocumented member.
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

-- | Undocumented member.
putAccountSettingDefaultResponse_setting :: Lens.Lens' PutAccountSettingDefaultResponse (Prelude.Maybe Setting)
putAccountSettingDefaultResponse_setting = Lens.lens (\PutAccountSettingDefaultResponse' {setting} -> setting) (\s@PutAccountSettingDefaultResponse' {} a -> s {setting = a} :: PutAccountSettingDefaultResponse)

-- | The response's http status code.
putAccountSettingDefaultResponse_httpStatus :: Lens.Lens' PutAccountSettingDefaultResponse Prelude.Int
putAccountSettingDefaultResponse_httpStatus = Lens.lens (\PutAccountSettingDefaultResponse' {httpStatus} -> httpStatus) (\s@PutAccountSettingDefaultResponse' {} a -> s {httpStatus = a} :: PutAccountSettingDefaultResponse)

instance
  Prelude.NFData
    PutAccountSettingDefaultResponse
