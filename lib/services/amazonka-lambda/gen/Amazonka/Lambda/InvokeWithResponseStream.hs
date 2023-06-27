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
-- Module      : Amazonka.Lambda.InvokeWithResponseStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configure your Lambda functions to stream response payloads back to
-- clients. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-response-streaming.html Configuring a Lambda function to stream responses>.
--
-- This operation requires permission for the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_awslambda.html lambda:InvokeFunction>
-- action. For details on how to set up permissions for cross-account
-- invocations, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html#permissions-resource-xaccountinvoke Granting function access to other accounts>.
module Amazonka.Lambda.InvokeWithResponseStream
  ( -- * Creating a Request
    InvokeWithResponseStream (..),
    newInvokeWithResponseStream,

    -- * Request Lenses
    invokeWithResponseStream_clientContext,
    invokeWithResponseStream_invocationType,
    invokeWithResponseStream_logType,
    invokeWithResponseStream_payload,
    invokeWithResponseStream_qualifier,
    invokeWithResponseStream_functionName,

    -- * Destructuring the Response
    InvokeWithResponseStreamResponse (..),
    newInvokeWithResponseStreamResponse,

    -- * Response Lenses
    invokeWithResponseStreamResponse_eventStream,
    invokeWithResponseStreamResponse_executedVersion,
    invokeWithResponseStreamResponse_responseStreamContentType,
    invokeWithResponseStreamResponse_statusCode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInvokeWithResponseStream' smart constructor.
data InvokeWithResponseStream = InvokeWithResponseStream'
  { -- | Up to 3,583 bytes of base64-encoded data about the invoking client to
    -- pass to the function in the context object.
    clientContext :: Prelude.Maybe Prelude.Text,
    -- | Use one of the following options:
    --
    -- -   @RequestResponse@ (default) – Invoke the function synchronously.
    --     Keep the connection open until the function returns a response or
    --     times out. The API operation response includes the function response
    --     and additional data.
    --
    -- -   @DryRun@ – Validate parameter values and verify that the IAM user or
    --     role has permission to invoke the function.
    invocationType :: Prelude.Maybe ResponseStreamingInvocationType,
    -- | Set to @Tail@ to include the execution log in the response. Applies to
    -- synchronously invoked functions only.
    logType :: Prelude.Maybe LogType,
    -- | The JSON that you want to provide to your Lambda function as input.
    --
    -- You can enter the JSON directly. For example,
    -- @--payload \'{ \"key\": \"value\" }\'@. You can also specify a file
    -- path. For example, @--payload file:\/\/payload.json@.
    payload :: Prelude.Maybe (Data.Sensitive Prelude.ByteString),
    -- | The alias name.
    qualifier :: Prelude.Maybe Prelude.Text,
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
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeWithResponseStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientContext', 'invokeWithResponseStream_clientContext' - Up to 3,583 bytes of base64-encoded data about the invoking client to
-- pass to the function in the context object.
--
-- 'invocationType', 'invokeWithResponseStream_invocationType' - Use one of the following options:
--
-- -   @RequestResponse@ (default) – Invoke the function synchronously.
--     Keep the connection open until the function returns a response or
--     times out. The API operation response includes the function response
--     and additional data.
--
-- -   @DryRun@ – Validate parameter values and verify that the IAM user or
--     role has permission to invoke the function.
--
-- 'logType', 'invokeWithResponseStream_logType' - Set to @Tail@ to include the execution log in the response. Applies to
-- synchronously invoked functions only.
--
-- 'payload', 'invokeWithResponseStream_payload' - The JSON that you want to provide to your Lambda function as input.
--
-- You can enter the JSON directly. For example,
-- @--payload \'{ \"key\": \"value\" }\'@. You can also specify a file
-- path. For example, @--payload file:\/\/payload.json@.
--
-- 'qualifier', 'invokeWithResponseStream_qualifier' - The alias name.
--
-- 'functionName', 'invokeWithResponseStream_functionName' - The name of the Lambda function.
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
newInvokeWithResponseStream ::
  -- | 'functionName'
  Prelude.Text ->
  InvokeWithResponseStream
newInvokeWithResponseStream pFunctionName_ =
  InvokeWithResponseStream'
    { clientContext =
        Prelude.Nothing,
      invocationType = Prelude.Nothing,
      logType = Prelude.Nothing,
      payload = Prelude.Nothing,
      qualifier = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Up to 3,583 bytes of base64-encoded data about the invoking client to
-- pass to the function in the context object.
invokeWithResponseStream_clientContext :: Lens.Lens' InvokeWithResponseStream (Prelude.Maybe Prelude.Text)
invokeWithResponseStream_clientContext = Lens.lens (\InvokeWithResponseStream' {clientContext} -> clientContext) (\s@InvokeWithResponseStream' {} a -> s {clientContext = a} :: InvokeWithResponseStream)

-- | Use one of the following options:
--
-- -   @RequestResponse@ (default) – Invoke the function synchronously.
--     Keep the connection open until the function returns a response or
--     times out. The API operation response includes the function response
--     and additional data.
--
-- -   @DryRun@ – Validate parameter values and verify that the IAM user or
--     role has permission to invoke the function.
invokeWithResponseStream_invocationType :: Lens.Lens' InvokeWithResponseStream (Prelude.Maybe ResponseStreamingInvocationType)
invokeWithResponseStream_invocationType = Lens.lens (\InvokeWithResponseStream' {invocationType} -> invocationType) (\s@InvokeWithResponseStream' {} a -> s {invocationType = a} :: InvokeWithResponseStream)

-- | Set to @Tail@ to include the execution log in the response. Applies to
-- synchronously invoked functions only.
invokeWithResponseStream_logType :: Lens.Lens' InvokeWithResponseStream (Prelude.Maybe LogType)
invokeWithResponseStream_logType = Lens.lens (\InvokeWithResponseStream' {logType} -> logType) (\s@InvokeWithResponseStream' {} a -> s {logType = a} :: InvokeWithResponseStream)

-- | The JSON that you want to provide to your Lambda function as input.
--
-- You can enter the JSON directly. For example,
-- @--payload \'{ \"key\": \"value\" }\'@. You can also specify a file
-- path. For example, @--payload file:\/\/payload.json@.
invokeWithResponseStream_payload :: Lens.Lens' InvokeWithResponseStream (Prelude.Maybe Prelude.ByteString)
invokeWithResponseStream_payload = Lens.lens (\InvokeWithResponseStream' {payload} -> payload) (\s@InvokeWithResponseStream' {} a -> s {payload = a} :: InvokeWithResponseStream) Prelude.. Lens.mapping Data._Sensitive

-- | The alias name.
invokeWithResponseStream_qualifier :: Lens.Lens' InvokeWithResponseStream (Prelude.Maybe Prelude.Text)
invokeWithResponseStream_qualifier = Lens.lens (\InvokeWithResponseStream' {qualifier} -> qualifier) (\s@InvokeWithResponseStream' {} a -> s {qualifier = a} :: InvokeWithResponseStream)

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
invokeWithResponseStream_functionName :: Lens.Lens' InvokeWithResponseStream Prelude.Text
invokeWithResponseStream_functionName = Lens.lens (\InvokeWithResponseStream' {functionName} -> functionName) (\s@InvokeWithResponseStream' {} a -> s {functionName = a} :: InvokeWithResponseStream)

instance Core.AWSRequest InvokeWithResponseStream where
  type
    AWSResponse InvokeWithResponseStream =
      InvokeWithResponseStreamResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InvokeWithResponseStreamResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (h Data..#? "X-Amz-Executed-Version")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InvokeWithResponseStream where
  hashWithSalt _salt InvokeWithResponseStream' {..} =
    _salt
      `Prelude.hashWithSalt` clientContext
      `Prelude.hashWithSalt` invocationType
      `Prelude.hashWithSalt` logType
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData InvokeWithResponseStream where
  rnf InvokeWithResponseStream' {..} =
    Prelude.rnf clientContext
      `Prelude.seq` Prelude.rnf invocationType
      `Prelude.seq` Prelude.rnf logType
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToBody InvokeWithResponseStream where
  toBody InvokeWithResponseStream' {..} =
    Data.toBody payload

instance Data.ToHeaders InvokeWithResponseStream where
  toHeaders InvokeWithResponseStream' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Context" Data.=# clientContext,
        "X-Amz-Invocation-Type" Data.=# invocationType,
        "X-Amz-Log-Type" Data.=# logType
      ]

instance Data.ToPath InvokeWithResponseStream where
  toPath InvokeWithResponseStream' {..} =
    Prelude.mconcat
      [ "/2021-11-15/functions/",
        Data.toBS functionName,
        "/response-streaming-invocations"
      ]

instance Data.ToQuery InvokeWithResponseStream where
  toQuery InvokeWithResponseStream' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newInvokeWithResponseStreamResponse' smart constructor.
data InvokeWithResponseStreamResponse = InvokeWithResponseStreamResponse'
  { -- | The stream of response payloads.
    eventStream :: Prelude.Maybe InvokeWithResponseStreamResponseEvent,
    -- | The version of the function that executed. When you invoke a function
    -- with an alias, this indicates which version the alias resolved to.
    executedVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of data the stream is returning.
    responseStreamContentType :: Prelude.Maybe Prelude.Text,
    -- | For a successful request, the HTTP status code is in the 200 range. For
    -- the @RequestResponse@ invocation type, this status code is 200. For the
    -- @DryRun@ invocation type, this status code is 204.
    statusCode :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeWithResponseStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventStream', 'invokeWithResponseStreamResponse_eventStream' - The stream of response payloads.
--
-- 'executedVersion', 'invokeWithResponseStreamResponse_executedVersion' - The version of the function that executed. When you invoke a function
-- with an alias, this indicates which version the alias resolved to.
--
-- 'responseStreamContentType', 'invokeWithResponseStreamResponse_responseStreamContentType' - The type of data the stream is returning.
--
-- 'statusCode', 'invokeWithResponseStreamResponse_statusCode' - For a successful request, the HTTP status code is in the 200 range. For
-- the @RequestResponse@ invocation type, this status code is 200. For the
-- @DryRun@ invocation type, this status code is 204.
newInvokeWithResponseStreamResponse ::
  -- | 'statusCode'
  Prelude.Int ->
  InvokeWithResponseStreamResponse
newInvokeWithResponseStreamResponse pStatusCode_ =
  InvokeWithResponseStreamResponse'
    { eventStream =
        Prelude.Nothing,
      executedVersion = Prelude.Nothing,
      responseStreamContentType =
        Prelude.Nothing,
      statusCode = pStatusCode_
    }

-- | The stream of response payloads.
invokeWithResponseStreamResponse_eventStream :: Lens.Lens' InvokeWithResponseStreamResponse (Prelude.Maybe InvokeWithResponseStreamResponseEvent)
invokeWithResponseStreamResponse_eventStream = Lens.lens (\InvokeWithResponseStreamResponse' {eventStream} -> eventStream) (\s@InvokeWithResponseStreamResponse' {} a -> s {eventStream = a} :: InvokeWithResponseStreamResponse)

-- | The version of the function that executed. When you invoke a function
-- with an alias, this indicates which version the alias resolved to.
invokeWithResponseStreamResponse_executedVersion :: Lens.Lens' InvokeWithResponseStreamResponse (Prelude.Maybe Prelude.Text)
invokeWithResponseStreamResponse_executedVersion = Lens.lens (\InvokeWithResponseStreamResponse' {executedVersion} -> executedVersion) (\s@InvokeWithResponseStreamResponse' {} a -> s {executedVersion = a} :: InvokeWithResponseStreamResponse)

-- | The type of data the stream is returning.
invokeWithResponseStreamResponse_responseStreamContentType :: Lens.Lens' InvokeWithResponseStreamResponse (Prelude.Maybe Prelude.Text)
invokeWithResponseStreamResponse_responseStreamContentType = Lens.lens (\InvokeWithResponseStreamResponse' {responseStreamContentType} -> responseStreamContentType) (\s@InvokeWithResponseStreamResponse' {} a -> s {responseStreamContentType = a} :: InvokeWithResponseStreamResponse)

-- | For a successful request, the HTTP status code is in the 200 range. For
-- the @RequestResponse@ invocation type, this status code is 200. For the
-- @DryRun@ invocation type, this status code is 204.
invokeWithResponseStreamResponse_statusCode :: Lens.Lens' InvokeWithResponseStreamResponse Prelude.Int
invokeWithResponseStreamResponse_statusCode = Lens.lens (\InvokeWithResponseStreamResponse' {statusCode} -> statusCode) (\s@InvokeWithResponseStreamResponse' {} a -> s {statusCode = a} :: InvokeWithResponseStreamResponse)

instance
  Prelude.NFData
    InvokeWithResponseStreamResponse
  where
  rnf InvokeWithResponseStreamResponse' {..} =
    Prelude.rnf eventStream
      `Prelude.seq` Prelude.rnf executedVersion
      `Prelude.seq` Prelude.rnf responseStreamContentType
      `Prelude.seq` Prelude.rnf statusCode
