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
-- Module      : Amazonka.Connect.StopContactStreaming
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends message streaming on a specified contact. To restart message
-- streaming on that contact, call the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_StartContactStreaming.html StartContactStreaming>
-- API.
module Amazonka.Connect.StopContactStreaming
  ( -- * Creating a Request
    StopContactStreaming (..),
    newStopContactStreaming,

    -- * Request Lenses
    stopContactStreaming_instanceId,
    stopContactStreaming_contactId,
    stopContactStreaming_streamingId,

    -- * Destructuring the Response
    StopContactStreamingResponse (..),
    newStopContactStreamingResponse,

    -- * Response Lenses
    stopContactStreamingResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopContactStreaming' smart constructor.
data StopContactStreaming = StopContactStreaming'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- that is associated with the first interaction with the contact center.
    contactId :: Prelude.Text,
    -- | The identifier of the streaming configuration enabled.
    streamingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopContactStreaming' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'stopContactStreaming_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'contactId', 'stopContactStreaming_contactId' - The identifier of the contact. This is the identifier of the contact
-- that is associated with the first interaction with the contact center.
--
-- 'streamingId', 'stopContactStreaming_streamingId' - The identifier of the streaming configuration enabled.
newStopContactStreaming ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'streamingId'
  Prelude.Text ->
  StopContactStreaming
newStopContactStreaming
  pInstanceId_
  pContactId_
  pStreamingId_ =
    StopContactStreaming'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        streamingId = pStreamingId_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
stopContactStreaming_instanceId :: Lens.Lens' StopContactStreaming Prelude.Text
stopContactStreaming_instanceId = Lens.lens (\StopContactStreaming' {instanceId} -> instanceId) (\s@StopContactStreaming' {} a -> s {instanceId = a} :: StopContactStreaming)

-- | The identifier of the contact. This is the identifier of the contact
-- that is associated with the first interaction with the contact center.
stopContactStreaming_contactId :: Lens.Lens' StopContactStreaming Prelude.Text
stopContactStreaming_contactId = Lens.lens (\StopContactStreaming' {contactId} -> contactId) (\s@StopContactStreaming' {} a -> s {contactId = a} :: StopContactStreaming)

-- | The identifier of the streaming configuration enabled.
stopContactStreaming_streamingId :: Lens.Lens' StopContactStreaming Prelude.Text
stopContactStreaming_streamingId = Lens.lens (\StopContactStreaming' {streamingId} -> streamingId) (\s@StopContactStreaming' {} a -> s {streamingId = a} :: StopContactStreaming)

instance Core.AWSRequest StopContactStreaming where
  type
    AWSResponse StopContactStreaming =
      StopContactStreamingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopContactStreamingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopContactStreaming where
  hashWithSalt _salt StopContactStreaming' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` streamingId

instance Prelude.NFData StopContactStreaming where
  rnf StopContactStreaming' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf streamingId

instance Data.ToHeaders StopContactStreaming where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopContactStreaming where
  toJSON StopContactStreaming' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just ("StreamingId" Data..= streamingId)
          ]
      )

instance Data.ToPath StopContactStreaming where
  toPath = Prelude.const "/contact/stop-streaming"

instance Data.ToQuery StopContactStreaming where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopContactStreamingResponse' smart constructor.
data StopContactStreamingResponse = StopContactStreamingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopContactStreamingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopContactStreamingResponse_httpStatus' - The response's http status code.
newStopContactStreamingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopContactStreamingResponse
newStopContactStreamingResponse pHttpStatus_ =
  StopContactStreamingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopContactStreamingResponse_httpStatus :: Lens.Lens' StopContactStreamingResponse Prelude.Int
stopContactStreamingResponse_httpStatus = Lens.lens (\StopContactStreamingResponse' {httpStatus} -> httpStatus) (\s@StopContactStreamingResponse' {} a -> s {httpStatus = a} :: StopContactStreamingResponse)

instance Prelude.NFData StopContactStreamingResponse where
  rnf StopContactStreamingResponse' {..} =
    Prelude.rnf httpStatus
