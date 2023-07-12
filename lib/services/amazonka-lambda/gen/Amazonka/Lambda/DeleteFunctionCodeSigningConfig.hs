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
-- Module      : Amazonka.Lambda.DeleteFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the code signing configuration from the function.
module Amazonka.Lambda.DeleteFunctionCodeSigningConfig
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteFunctionCodeSigningConfig
  where
  type
    AWSResponse DeleteFunctionCodeSigningConfig =
      DeleteFunctionCodeSigningConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteFunctionCodeSigningConfigResponse'

instance
  Prelude.Hashable
    DeleteFunctionCodeSigningConfig
  where
  hashWithSalt
    _salt
    DeleteFunctionCodeSigningConfig' {..} =
      _salt `Prelude.hashWithSalt` functionName

instance
  Prelude.NFData
    DeleteFunctionCodeSigningConfig
  where
  rnf DeleteFunctionCodeSigningConfig' {..} =
    Prelude.rnf functionName

instance
  Data.ToHeaders
    DeleteFunctionCodeSigningConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFunctionCodeSigningConfig where
  toPath DeleteFunctionCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-06-30/functions/",
        Data.toBS functionName,
        "/code-signing-config"
      ]

instance Data.ToQuery DeleteFunctionCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFunctionCodeSigningConfigResponse' smart constructor.
data DeleteFunctionCodeSigningConfigResponse = DeleteFunctionCodeSigningConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
