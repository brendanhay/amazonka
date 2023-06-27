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
-- Module      : Amazonka.Connect.StartContactStreaming
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates real-time message streaming for a new chat contact.
--
-- For more information about message streaming, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/chat-message-streaming.html Enable real-time chat message streaming>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.StartContactStreaming
  ( -- * Creating a Request
    StartContactStreaming (..),
    newStartContactStreaming,

    -- * Request Lenses
    startContactStreaming_instanceId,
    startContactStreaming_contactId,
    startContactStreaming_chatStreamingConfiguration,
    startContactStreaming_clientToken,

    -- * Destructuring the Response
    StartContactStreamingResponse (..),
    newStartContactStreamingResponse,

    -- * Response Lenses
    startContactStreamingResponse_httpStatus,
    startContactStreamingResponse_streamingId,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartContactStreaming' smart constructor.
data StartContactStreaming = StartContactStreaming'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    contactId :: Prelude.Text,
    -- | The streaming configuration, such as the Amazon SNS streaming endpoint.
    chatStreamingConfiguration :: ChatStreamingConfiguration,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContactStreaming' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'startContactStreaming_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'contactId', 'startContactStreaming_contactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
--
-- 'chatStreamingConfiguration', 'startContactStreaming_chatStreamingConfiguration' - The streaming configuration, such as the Amazon SNS streaming endpoint.
--
-- 'clientToken', 'startContactStreaming_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
newStartContactStreaming ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'chatStreamingConfiguration'
  ChatStreamingConfiguration ->
  -- | 'clientToken'
  Prelude.Text ->
  StartContactStreaming
newStartContactStreaming
  pInstanceId_
  pContactId_
  pChatStreamingConfiguration_
  pClientToken_ =
    StartContactStreaming'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        chatStreamingConfiguration =
          pChatStreamingConfiguration_,
        clientToken = pClientToken_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
startContactStreaming_instanceId :: Lens.Lens' StartContactStreaming Prelude.Text
startContactStreaming_instanceId = Lens.lens (\StartContactStreaming' {instanceId} -> instanceId) (\s@StartContactStreaming' {} a -> s {instanceId = a} :: StartContactStreaming)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
startContactStreaming_contactId :: Lens.Lens' StartContactStreaming Prelude.Text
startContactStreaming_contactId = Lens.lens (\StartContactStreaming' {contactId} -> contactId) (\s@StartContactStreaming' {} a -> s {contactId = a} :: StartContactStreaming)

-- | The streaming configuration, such as the Amazon SNS streaming endpoint.
startContactStreaming_chatStreamingConfiguration :: Lens.Lens' StartContactStreaming ChatStreamingConfiguration
startContactStreaming_chatStreamingConfiguration = Lens.lens (\StartContactStreaming' {chatStreamingConfiguration} -> chatStreamingConfiguration) (\s@StartContactStreaming' {} a -> s {chatStreamingConfiguration = a} :: StartContactStreaming)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
startContactStreaming_clientToken :: Lens.Lens' StartContactStreaming Prelude.Text
startContactStreaming_clientToken = Lens.lens (\StartContactStreaming' {clientToken} -> clientToken) (\s@StartContactStreaming' {} a -> s {clientToken = a} :: StartContactStreaming)

instance Core.AWSRequest StartContactStreaming where
  type
    AWSResponse StartContactStreaming =
      StartContactStreamingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContactStreamingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "StreamingId")
      )

instance Prelude.Hashable StartContactStreaming where
  hashWithSalt _salt StartContactStreaming' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` chatStreamingConfiguration
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartContactStreaming where
  rnf StartContactStreaming' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf chatStreamingConfiguration
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartContactStreaming where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartContactStreaming where
  toJSON StartContactStreaming' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just
              ( "ChatStreamingConfiguration"
                  Data..= chatStreamingConfiguration
              ),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartContactStreaming where
  toPath = Prelude.const "/contact/start-streaming"

instance Data.ToQuery StartContactStreaming where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContactStreamingResponse' smart constructor.
data StartContactStreamingResponse = StartContactStreamingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the streaming configuration enabled.
    streamingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContactStreamingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startContactStreamingResponse_httpStatus' - The response's http status code.
--
-- 'streamingId', 'startContactStreamingResponse_streamingId' - The identifier of the streaming configuration enabled.
newStartContactStreamingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'streamingId'
  Prelude.Text ->
  StartContactStreamingResponse
newStartContactStreamingResponse
  pHttpStatus_
  pStreamingId_ =
    StartContactStreamingResponse'
      { httpStatus =
          pHttpStatus_,
        streamingId = pStreamingId_
      }

-- | The response's http status code.
startContactStreamingResponse_httpStatus :: Lens.Lens' StartContactStreamingResponse Prelude.Int
startContactStreamingResponse_httpStatus = Lens.lens (\StartContactStreamingResponse' {httpStatus} -> httpStatus) (\s@StartContactStreamingResponse' {} a -> s {httpStatus = a} :: StartContactStreamingResponse)

-- | The identifier of the streaming configuration enabled.
startContactStreamingResponse_streamingId :: Lens.Lens' StartContactStreamingResponse Prelude.Text
startContactStreamingResponse_streamingId = Lens.lens (\StartContactStreamingResponse' {streamingId} -> streamingId) (\s@StartContactStreamingResponse' {} a -> s {streamingId = a} :: StartContactStreamingResponse)

instance Prelude.NFData StartContactStreamingResponse where
  rnf StartContactStreamingResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf streamingId
