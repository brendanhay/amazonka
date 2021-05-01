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
-- Module      : Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration for asynchronous invocation for a function,
-- version, or alias.
--
-- To configure options for asynchronous invocation, use
-- PutFunctionEventInvokeConfig.
module Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
  ( -- * Creating a Request
    DeleteFunctionEventInvokeConfig (..),
    newDeleteFunctionEventInvokeConfig,

    -- * Request Lenses
    deleteFunctionEventInvokeConfig_qualifier,
    deleteFunctionEventInvokeConfig_functionName,

    -- * Destructuring the Response
    DeleteFunctionEventInvokeConfigResponse (..),
    newDeleteFunctionEventInvokeConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFunctionEventInvokeConfig' smart constructor.
data DeleteFunctionEventInvokeConfig = DeleteFunctionEventInvokeConfig'
  { -- | A version number or alias name.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'deleteFunctionEventInvokeConfig_qualifier' - A version number or alias name.
--
-- 'functionName', 'deleteFunctionEventInvokeConfig_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newDeleteFunctionEventInvokeConfig ::
  -- | 'functionName'
  Prelude.Text ->
  DeleteFunctionEventInvokeConfig
newDeleteFunctionEventInvokeConfig pFunctionName_ =
  DeleteFunctionEventInvokeConfig'
    { qualifier =
        Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | A version number or alias name.
deleteFunctionEventInvokeConfig_qualifier :: Lens.Lens' DeleteFunctionEventInvokeConfig (Prelude.Maybe Prelude.Text)
deleteFunctionEventInvokeConfig_qualifier = Lens.lens (\DeleteFunctionEventInvokeConfig' {qualifier} -> qualifier) (\s@DeleteFunctionEventInvokeConfig' {} a -> s {qualifier = a} :: DeleteFunctionEventInvokeConfig)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
deleteFunctionEventInvokeConfig_functionName :: Lens.Lens' DeleteFunctionEventInvokeConfig Prelude.Text
deleteFunctionEventInvokeConfig_functionName = Lens.lens (\DeleteFunctionEventInvokeConfig' {functionName} -> functionName) (\s@DeleteFunctionEventInvokeConfig' {} a -> s {functionName = a} :: DeleteFunctionEventInvokeConfig)

instance
  Prelude.AWSRequest
    DeleteFunctionEventInvokeConfig
  where
  type
    Rs DeleteFunctionEventInvokeConfig =
      DeleteFunctionEventInvokeConfigResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteFunctionEventInvokeConfigResponse'

instance
  Prelude.Hashable
    DeleteFunctionEventInvokeConfig

instance
  Prelude.NFData
    DeleteFunctionEventInvokeConfig

instance
  Prelude.ToHeaders
    DeleteFunctionEventInvokeConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteFunctionEventInvokeConfig
  where
  toPath DeleteFunctionEventInvokeConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-25/functions/",
        Prelude.toBS functionName,
        "/event-invoke-config"
      ]

instance
  Prelude.ToQuery
    DeleteFunctionEventInvokeConfig
  where
  toQuery DeleteFunctionEventInvokeConfig' {..} =
    Prelude.mconcat ["Qualifier" Prelude.=: qualifier]

-- | /See:/ 'newDeleteFunctionEventInvokeConfigResponse' smart constructor.
data DeleteFunctionEventInvokeConfigResponse = DeleteFunctionEventInvokeConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionEventInvokeConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFunctionEventInvokeConfigResponse ::
  DeleteFunctionEventInvokeConfigResponse
newDeleteFunctionEventInvokeConfigResponse =
  DeleteFunctionEventInvokeConfigResponse'

instance
  Prelude.NFData
    DeleteFunctionEventInvokeConfigResponse
