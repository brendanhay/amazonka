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
-- Module      : Amazonka.Lambda.GetFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration for asynchronous invocation for a function,
-- version, or alias.
--
-- To configure options for asynchronous invocation, use
-- PutFunctionEventInvokeConfig.
module Amazonka.Lambda.GetFunctionEventInvokeConfig
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
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFunctionEventInvokeConfig' smart constructor.
data GetFunctionEventInvokeConfig = GetFunctionEventInvokeConfig'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetFunctionEventInvokeConfig
newGetFunctionEventInvokeConfig pFunctionName_ =
  GetFunctionEventInvokeConfig'
    { qualifier =
        Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | A version number or alias name.
getFunctionEventInvokeConfig_qualifier :: Lens.Lens' GetFunctionEventInvokeConfig (Prelude.Maybe Prelude.Text)
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
getFunctionEventInvokeConfig_functionName :: Lens.Lens' GetFunctionEventInvokeConfig Prelude.Text
getFunctionEventInvokeConfig_functionName = Lens.lens (\GetFunctionEventInvokeConfig' {functionName} -> functionName) (\s@GetFunctionEventInvokeConfig' {} a -> s {functionName = a} :: GetFunctionEventInvokeConfig)

instance Core.AWSRequest GetFunctionEventInvokeConfig where
  type
    AWSResponse GetFunctionEventInvokeConfig =
      FunctionEventInvokeConfig
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    GetFunctionEventInvokeConfig
  where
  hashWithSalt _salt GetFunctionEventInvokeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetFunctionEventInvokeConfig where
  rnf GetFunctionEventInvokeConfig' {..} =
    Prelude.rnf qualifier `Prelude.seq`
      Prelude.rnf functionName

instance Data.ToHeaders GetFunctionEventInvokeConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFunctionEventInvokeConfig where
  toPath GetFunctionEventInvokeConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-25/functions/",
        Data.toBS functionName,
        "/event-invoke-config"
      ]

instance Data.ToQuery GetFunctionEventInvokeConfig where
  toQuery GetFunctionEventInvokeConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]
