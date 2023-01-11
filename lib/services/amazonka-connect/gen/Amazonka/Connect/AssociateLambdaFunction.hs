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
-- Module      : Amazonka.Connect.AssociateLambdaFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Allows the specified Amazon Connect instance to access the specified
-- Lambda function.
module Amazonka.Connect.AssociateLambdaFunction
  ( -- * Creating a Request
    AssociateLambdaFunction (..),
    newAssociateLambdaFunction,

    -- * Request Lenses
    associateLambdaFunction_instanceId,
    associateLambdaFunction_functionArn,

    -- * Destructuring the Response
    AssociateLambdaFunctionResponse (..),
    newAssociateLambdaFunctionResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateLambdaFunction' smart constructor.
data AssociateLambdaFunction = AssociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the Lambda function being associated.
    -- Maximum number of characters allowed is 140.
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLambdaFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateLambdaFunction_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'functionArn', 'associateLambdaFunction_functionArn' - The Amazon Resource Name (ARN) for the Lambda function being associated.
-- Maximum number of characters allowed is 140.
newAssociateLambdaFunction ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'functionArn'
  Prelude.Text ->
  AssociateLambdaFunction
newAssociateLambdaFunction pInstanceId_ pFunctionArn_ =
  AssociateLambdaFunction'
    { instanceId = pInstanceId_,
      functionArn = pFunctionArn_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
associateLambdaFunction_instanceId :: Lens.Lens' AssociateLambdaFunction Prelude.Text
associateLambdaFunction_instanceId = Lens.lens (\AssociateLambdaFunction' {instanceId} -> instanceId) (\s@AssociateLambdaFunction' {} a -> s {instanceId = a} :: AssociateLambdaFunction)

-- | The Amazon Resource Name (ARN) for the Lambda function being associated.
-- Maximum number of characters allowed is 140.
associateLambdaFunction_functionArn :: Lens.Lens' AssociateLambdaFunction Prelude.Text
associateLambdaFunction_functionArn = Lens.lens (\AssociateLambdaFunction' {functionArn} -> functionArn) (\s@AssociateLambdaFunction' {} a -> s {functionArn = a} :: AssociateLambdaFunction)

instance Core.AWSRequest AssociateLambdaFunction where
  type
    AWSResponse AssociateLambdaFunction =
      AssociateLambdaFunctionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      AssociateLambdaFunctionResponse'

instance Prelude.Hashable AssociateLambdaFunction where
  hashWithSalt _salt AssociateLambdaFunction' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` functionArn

instance Prelude.NFData AssociateLambdaFunction where
  rnf AssociateLambdaFunction' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf functionArn

instance Data.ToHeaders AssociateLambdaFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateLambdaFunction where
  toJSON AssociateLambdaFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FunctionArn" Data..= functionArn)]
      )

instance Data.ToPath AssociateLambdaFunction where
  toPath AssociateLambdaFunction' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/lambda-function"
      ]

instance Data.ToQuery AssociateLambdaFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateLambdaFunctionResponse' smart constructor.
data AssociateLambdaFunctionResponse = AssociateLambdaFunctionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLambdaFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateLambdaFunctionResponse ::
  AssociateLambdaFunctionResponse
newAssociateLambdaFunctionResponse =
  AssociateLambdaFunctionResponse'

instance
  Prelude.NFData
    AssociateLambdaFunctionResponse
  where
  rnf _ = ()
