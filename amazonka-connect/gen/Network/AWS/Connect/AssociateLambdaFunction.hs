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
-- Module      : Network.AWS.Connect.AssociateLambdaFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Connect.AssociateLambdaFunction
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateLambdaFunction' smart constructor.
data AssociateLambdaFunction = AssociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The Amazon Resource Name (ARN) for the Lambda function being associated.
    -- Maximum number of characters allowed is 140.
    functionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateLambdaFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateLambdaFunction_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'functionArn', 'associateLambdaFunction_functionArn' - The Amazon Resource Name (ARN) for the Lambda function being associated.
-- Maximum number of characters allowed is 140.
newAssociateLambdaFunction ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'functionArn'
  Core.Text ->
  AssociateLambdaFunction
newAssociateLambdaFunction pInstanceId_ pFunctionArn_ =
  AssociateLambdaFunction'
    { instanceId = pInstanceId_,
      functionArn = pFunctionArn_
    }

-- | The identifier of the Amazon Connect instance.
associateLambdaFunction_instanceId :: Lens.Lens' AssociateLambdaFunction Core.Text
associateLambdaFunction_instanceId = Lens.lens (\AssociateLambdaFunction' {instanceId} -> instanceId) (\s@AssociateLambdaFunction' {} a -> s {instanceId = a} :: AssociateLambdaFunction)

-- | The Amazon Resource Name (ARN) for the Lambda function being associated.
-- Maximum number of characters allowed is 140.
associateLambdaFunction_functionArn :: Lens.Lens' AssociateLambdaFunction Core.Text
associateLambdaFunction_functionArn = Lens.lens (\AssociateLambdaFunction' {functionArn} -> functionArn) (\s@AssociateLambdaFunction' {} a -> s {functionArn = a} :: AssociateLambdaFunction)

instance Core.AWSRequest AssociateLambdaFunction where
  type
    AWSResponse AssociateLambdaFunction =
      AssociateLambdaFunctionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull
      AssociateLambdaFunctionResponse'

instance Core.Hashable AssociateLambdaFunction

instance Core.NFData AssociateLambdaFunction

instance Core.ToHeaders AssociateLambdaFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateLambdaFunction where
  toJSON AssociateLambdaFunction' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FunctionArn" Core..= functionArn)]
      )

instance Core.ToPath AssociateLambdaFunction where
  toPath AssociateLambdaFunction' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/lambda-function"
      ]

instance Core.ToQuery AssociateLambdaFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateLambdaFunctionResponse' smart constructor.
data AssociateLambdaFunctionResponse = AssociateLambdaFunctionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateLambdaFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateLambdaFunctionResponse ::
  AssociateLambdaFunctionResponse
newAssociateLambdaFunctionResponse =
  AssociateLambdaFunctionResponse'

instance Core.NFData AssociateLambdaFunctionResponse
