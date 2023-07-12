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
-- Module      : Amazonka.Connect.DisassociateLambdaFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Remove the Lambda function from the dropdown options available in the
-- relevant flow blocks.
module Amazonka.Connect.DisassociateLambdaFunction
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateLambdaFunction' smart constructor.
data DisassociateLambdaFunction = DisassociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance..
    instanceId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Lambda function being
    -- disassociated.
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLambdaFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateLambdaFunction_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance..
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance..
disassociateLambdaFunction_instanceId :: Lens.Lens' DisassociateLambdaFunction Prelude.Text
disassociateLambdaFunction_instanceId = Lens.lens (\DisassociateLambdaFunction' {instanceId} -> instanceId) (\s@DisassociateLambdaFunction' {} a -> s {instanceId = a} :: DisassociateLambdaFunction)

-- | The Amazon Resource Name (ARN) of the Lambda function being
-- disassociated.
disassociateLambdaFunction_functionArn :: Lens.Lens' DisassociateLambdaFunction Prelude.Text
disassociateLambdaFunction_functionArn = Lens.lens (\DisassociateLambdaFunction' {functionArn} -> functionArn) (\s@DisassociateLambdaFunction' {} a -> s {functionArn = a} :: DisassociateLambdaFunction)

instance Core.AWSRequest DisassociateLambdaFunction where
  type
    AWSResponse DisassociateLambdaFunction =
      DisassociateLambdaFunctionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateLambdaFunctionResponse'

instance Prelude.Hashable DisassociateLambdaFunction where
  hashWithSalt _salt DisassociateLambdaFunction' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` functionArn

instance Prelude.NFData DisassociateLambdaFunction where
  rnf DisassociateLambdaFunction' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf functionArn

instance Data.ToHeaders DisassociateLambdaFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateLambdaFunction where
  toPath DisassociateLambdaFunction' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/lambda-function"
      ]

instance Data.ToQuery DisassociateLambdaFunction where
  toQuery DisassociateLambdaFunction' {..} =
    Prelude.mconcat ["functionArn" Data.=: functionArn]

-- | /See:/ 'newDisassociateLambdaFunctionResponse' smart constructor.
data DisassociateLambdaFunctionResponse = DisassociateLambdaFunctionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
