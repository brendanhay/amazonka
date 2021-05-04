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
-- Module      : Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the code signing configuration from the function.
module Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
  ( -- * Creating a Request
    DeleteFunctionCodeSigningConfig (..),
    newDeleteFunctionCodeSigningConfig,

    -- * Request Lenses
    deleteFunctionCodeSigningConfig_functionName,

    -- * Destructuring the Response
    DeleteFunctionCodeSigningConfigResponse (..),
    newDeleteFunctionCodeSigningConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFunctionCodeSigningConfig' smart constructor.
data DeleteFunctionCodeSigningConfig = DeleteFunctionCodeSigningConfig'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'deleteFunctionCodeSigningConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newDeleteFunctionCodeSigningConfig ::
  -- | 'functionName'
  Prelude.Text ->
  DeleteFunctionCodeSigningConfig
newDeleteFunctionCodeSigningConfig pFunctionName_ =
  DeleteFunctionCodeSigningConfig'
    { functionName =
        pFunctionName_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
deleteFunctionCodeSigningConfig_functionName :: Lens.Lens' DeleteFunctionCodeSigningConfig Prelude.Text
deleteFunctionCodeSigningConfig_functionName = Lens.lens (\DeleteFunctionCodeSigningConfig' {functionName} -> functionName) (\s@DeleteFunctionCodeSigningConfig' {} a -> s {functionName = a} :: DeleteFunctionCodeSigningConfig)

instance
  Prelude.AWSRequest
    DeleteFunctionCodeSigningConfig
  where
  type
    Rs DeleteFunctionCodeSigningConfig =
      DeleteFunctionCodeSigningConfigResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteFunctionCodeSigningConfigResponse'

instance
  Prelude.Hashable
    DeleteFunctionCodeSigningConfig

instance
  Prelude.NFData
    DeleteFunctionCodeSigningConfig

instance
  Prelude.ToHeaders
    DeleteFunctionCodeSigningConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteFunctionCodeSigningConfig
  where
  toPath DeleteFunctionCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-06-30/functions/",
        Prelude.toBS functionName,
        "/code-signing-config"
      ]

instance
  Prelude.ToQuery
    DeleteFunctionCodeSigningConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFunctionCodeSigningConfigResponse' smart constructor.
data DeleteFunctionCodeSigningConfigResponse = DeleteFunctionCodeSigningConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFunctionCodeSigningConfigResponse ::
  DeleteFunctionCodeSigningConfigResponse
newDeleteFunctionCodeSigningConfigResponse =
  DeleteFunctionCodeSigningConfigResponse'

instance
  Prelude.NFData
    DeleteFunctionCodeSigningConfigResponse
