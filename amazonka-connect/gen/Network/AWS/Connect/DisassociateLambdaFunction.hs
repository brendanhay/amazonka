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
-- Module      : Network.AWS.Connect.DisassociateLambdaFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Remove the Lambda function from the dropdown options available in the
-- relevant contact flow blocks.
module Network.AWS.Connect.DisassociateLambdaFunction
  ( -- * Creating a Request
    DisassociateLambdaFunction (..),
    newDisassociateLambdaFunction,

    -- * Request Lenses
    disassociateLambdaFunction_instanceId,
    disassociateLambdaFunction_functionArn,

    -- * Destructuring the Response
    DisassociateLambdaFunctionResponse (..),
    newDisassociateLambdaFunctionResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateLambdaFunction' smart constructor.
data DisassociateLambdaFunction = DisassociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance..
    instanceId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Lambda function being
    -- disassociated.
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLambdaFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateLambdaFunction_instanceId' - The identifier of the Amazon Connect instance..
--
-- 'functionArn', 'disassociateLambdaFunction_functionArn' - The Amazon Resource Name (ARN) of the Lambda function being
-- disassociated.
newDisassociateLambdaFunction ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'functionArn'
  Prelude.Text ->
  DisassociateLambdaFunction
newDisassociateLambdaFunction
  pInstanceId_
  pFunctionArn_ =
    DisassociateLambdaFunction'
      { instanceId =
          pInstanceId_,
        functionArn = pFunctionArn_
      }

-- | The identifier of the Amazon Connect instance..
disassociateLambdaFunction_instanceId :: Lens.Lens' DisassociateLambdaFunction Prelude.Text
disassociateLambdaFunction_instanceId = Lens.lens (\DisassociateLambdaFunction' {instanceId} -> instanceId) (\s@DisassociateLambdaFunction' {} a -> s {instanceId = a} :: DisassociateLambdaFunction)

-- | The Amazon Resource Name (ARN) of the Lambda function being
-- disassociated.
disassociateLambdaFunction_functionArn :: Lens.Lens' DisassociateLambdaFunction Prelude.Text
disassociateLambdaFunction_functionArn = Lens.lens (\DisassociateLambdaFunction' {functionArn} -> functionArn) (\s@DisassociateLambdaFunction' {} a -> s {functionArn = a} :: DisassociateLambdaFunction)

instance
  Prelude.AWSRequest
    DisassociateLambdaFunction
  where
  type
    Rs DisassociateLambdaFunction =
      DisassociateLambdaFunctionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DisassociateLambdaFunctionResponse'

instance Prelude.Hashable DisassociateLambdaFunction

instance Prelude.NFData DisassociateLambdaFunction

instance Prelude.ToHeaders DisassociateLambdaFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DisassociateLambdaFunction where
  toPath DisassociateLambdaFunction' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/lambda-function"
      ]

instance Prelude.ToQuery DisassociateLambdaFunction where
  toQuery DisassociateLambdaFunction' {..} =
    Prelude.mconcat
      ["functionArn" Prelude.=: functionArn]

-- | /See:/ 'newDisassociateLambdaFunctionResponse' smart constructor.
data DisassociateLambdaFunctionResponse = DisassociateLambdaFunctionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLambdaFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateLambdaFunctionResponse ::
  DisassociateLambdaFunctionResponse
newDisassociateLambdaFunctionResponse =
  DisassociateLambdaFunctionResponse'

instance
  Prelude.NFData
    DisassociateLambdaFunctionResponse
