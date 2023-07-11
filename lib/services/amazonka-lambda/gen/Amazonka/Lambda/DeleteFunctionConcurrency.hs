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
-- Module      : Amazonka.Lambda.DeleteFunctionConcurrency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a concurrent execution limit from a function.
module Amazonka.Lambda.DeleteFunctionConcurrency
  ( -- * Creating a Request
    DeleteFunctionConcurrency (..),
    newDeleteFunctionConcurrency,

    -- * Request Lenses
    deleteFunctionConcurrency_functionName,

    -- * Destructuring the Response
    DeleteFunctionConcurrencyResponse (..),
    newDeleteFunctionConcurrencyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFunctionConcurrency' smart constructor.
data DeleteFunctionConcurrency = DeleteFunctionConcurrency'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionConcurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'deleteFunctionConcurrency_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newDeleteFunctionConcurrency ::
  -- | 'functionName'
  Prelude.Text ->
  DeleteFunctionConcurrency
newDeleteFunctionConcurrency pFunctionName_ =
  DeleteFunctionConcurrency'
    { functionName =
        pFunctionName_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
deleteFunctionConcurrency_functionName :: Lens.Lens' DeleteFunctionConcurrency Prelude.Text
deleteFunctionConcurrency_functionName = Lens.lens (\DeleteFunctionConcurrency' {functionName} -> functionName) (\s@DeleteFunctionConcurrency' {} a -> s {functionName = a} :: DeleteFunctionConcurrency)

instance Core.AWSRequest DeleteFunctionConcurrency where
  type
    AWSResponse DeleteFunctionConcurrency =
      DeleteFunctionConcurrencyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteFunctionConcurrencyResponse'

instance Prelude.Hashable DeleteFunctionConcurrency where
  hashWithSalt _salt DeleteFunctionConcurrency' {..} =
    _salt `Prelude.hashWithSalt` functionName

instance Prelude.NFData DeleteFunctionConcurrency where
  rnf DeleteFunctionConcurrency' {..} =
    Prelude.rnf functionName

instance Data.ToHeaders DeleteFunctionConcurrency where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFunctionConcurrency where
  toPath DeleteFunctionConcurrency' {..} =
    Prelude.mconcat
      [ "/2017-10-31/functions/",
        Data.toBS functionName,
        "/concurrency"
      ]

instance Data.ToQuery DeleteFunctionConcurrency where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFunctionConcurrencyResponse' smart constructor.
data DeleteFunctionConcurrencyResponse = DeleteFunctionConcurrencyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionConcurrencyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFunctionConcurrencyResponse ::
  DeleteFunctionConcurrencyResponse
newDeleteFunctionConcurrencyResponse =
  DeleteFunctionConcurrencyResponse'

instance
  Prelude.NFData
    DeleteFunctionConcurrencyResponse
  where
  rnf _ = ()
