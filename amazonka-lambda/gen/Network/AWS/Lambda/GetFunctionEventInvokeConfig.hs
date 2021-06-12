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
-- Module      : Network.AWS.Lambda.GetFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration for asynchronous invocation for a function,
-- version, or alias.
--
-- To configure options for asynchronous invocation, use
-- PutFunctionEventInvokeConfig.
module Network.AWS.Lambda.GetFunctionEventInvokeConfig
  ( -- * Creating a Request
    GetFunctionEventInvokeConfig (..),
    newGetFunctionEventInvokeConfig,

    -- * Request Lenses
    getFunctionEventInvokeConfig_qualifier,
    getFunctionEventInvokeConfig_functionName,

    -- * Destructuring the Response
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,

    -- * Response Lenses
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFunctionEventInvokeConfig' smart constructor.
data GetFunctionEventInvokeConfig = GetFunctionEventInvokeConfig'
  { -- | A version number or alias name.
    qualifier :: Core.Maybe Core.Text,
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
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'getFunctionEventInvokeConfig_qualifier' - A version number or alias name.
--
-- 'functionName', 'getFunctionEventInvokeConfig_functionName' - The name of the Lambda function, version, or alias.
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
newGetFunctionEventInvokeConfig ::
  -- | 'functionName'
  Core.Text ->
  GetFunctionEventInvokeConfig
newGetFunctionEventInvokeConfig pFunctionName_ =
  GetFunctionEventInvokeConfig'
    { qualifier =
        Core.Nothing,
      functionName = pFunctionName_
    }

-- | A version number or alias name.
getFunctionEventInvokeConfig_qualifier :: Lens.Lens' GetFunctionEventInvokeConfig (Core.Maybe Core.Text)
getFunctionEventInvokeConfig_qualifier = Lens.lens (\GetFunctionEventInvokeConfig' {qualifier} -> qualifier) (\s@GetFunctionEventInvokeConfig' {} a -> s {qualifier = a} :: GetFunctionEventInvokeConfig)

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
getFunctionEventInvokeConfig_functionName :: Lens.Lens' GetFunctionEventInvokeConfig Core.Text
getFunctionEventInvokeConfig_functionName = Lens.lens (\GetFunctionEventInvokeConfig' {functionName} -> functionName) (\s@GetFunctionEventInvokeConfig' {} a -> s {functionName = a} :: GetFunctionEventInvokeConfig)

instance Core.AWSRequest GetFunctionEventInvokeConfig where
  type
    AWSResponse GetFunctionEventInvokeConfig =
      FunctionEventInvokeConfig
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetFunctionEventInvokeConfig

instance Core.NFData GetFunctionEventInvokeConfig

instance Core.ToHeaders GetFunctionEventInvokeConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetFunctionEventInvokeConfig where
  toPath GetFunctionEventInvokeConfig' {..} =
    Core.mconcat
      [ "/2019-09-25/functions/",
        Core.toBS functionName,
        "/event-invoke-config"
      ]

instance Core.ToQuery GetFunctionEventInvokeConfig where
  toQuery GetFunctionEventInvokeConfig' {..} =
    Core.mconcat ["Qualifier" Core.=: qualifier]
