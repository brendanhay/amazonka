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
-- Module      : Amazonka.IVSChat.UpdateRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a room’s configuration.
module Amazonka.IVSChat.UpdateRoom
  ( -- * Creating a Request
    UpdateRoom (..),
    newUpdateRoom,

    -- * Request Lenses
    updateRoom_loggingConfigurationIdentifiers,
    updateRoom_maximumMessageLength,
    updateRoom_maximumMessageRatePerSecond,
    updateRoom_messageReviewHandler,
    updateRoom_name,
    updateRoom_identifier,

    -- * Destructuring the Response
    UpdateRoomResponse (..),
    newUpdateRoomResponse,

    -- * Response Lenses
    updateRoomResponse_arn,
    updateRoomResponse_createTime,
    updateRoomResponse_id,
    updateRoomResponse_loggingConfigurationIdentifiers,
    updateRoomResponse_maximumMessageLength,
    updateRoomResponse_maximumMessageRatePerSecond,
    updateRoomResponse_messageReviewHandler,
    updateRoomResponse_name,
    updateRoomResponse_tags,
    updateRoomResponse_updateTime,
    updateRoomResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { -- | Array of logging-configuration identifiers attached to the room.
    loggingConfigurationIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of characters in a single message. Messages are
    -- expected to be UTF-8 encoded and this limit applies specifically to
    -- rune\/code-point count, not number of bytes. Default: 500.
    maximumMessageLength :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of messages per second that can be sent to the room (by
    -- all clients). Default: 10.
    maximumMessageRatePerSecond :: Prelude.Maybe Prelude.Natural,
    -- | Configuration information for optional review of messages. Specify an
    -- empty @uri@ string to disassociate a message review handler from the
    -- specified room.
    messageReviewHandler :: Prelude.Maybe MessageReviewHandler,
    -- | Room name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the room to be updated. Currently this must be an ARN.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfigurationIdentifiers', 'updateRoom_loggingConfigurationIdentifiers' - Array of logging-configuration identifiers attached to the room.
--
-- 'maximumMessageLength', 'updateRoom_maximumMessageLength' - The maximum number of characters in a single message. Messages are
-- expected to be UTF-8 encoded and this limit applies specifically to
-- rune\/code-point count, not number of bytes. Default: 500.
--
-- 'maximumMessageRatePerSecond', 'updateRoom_maximumMessageRatePerSecond' - Maximum number of messages per second that can be sent to the room (by
-- all clients). Default: 10.
--
-- 'messageReviewHandler', 'updateRoom_messageReviewHandler' - Configuration information for optional review of messages. Specify an
-- empty @uri@ string to disassociate a message review handler from the
-- specified room.
--
-- 'name', 'updateRoom_name' - Room name. The value does not need to be unique.
--
-- 'identifier', 'updateRoom_identifier' - Identifier of the room to be updated. Currently this must be an ARN.
newUpdateRoom ::
  -- | 'identifier'
  Prelude.Text ->
  UpdateRoom
newUpdateRoom pIdentifier_ =
  UpdateRoom'
    { loggingConfigurationIdentifiers =
        Prelude.Nothing,
      maximumMessageLength = Prelude.Nothing,
      maximumMessageRatePerSecond = Prelude.Nothing,
      messageReviewHandler = Prelude.Nothing,
      name = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | Array of logging-configuration identifiers attached to the room.
updateRoom_loggingConfigurationIdentifiers :: Lens.Lens' UpdateRoom (Prelude.Maybe [Prelude.Text])
updateRoom_loggingConfigurationIdentifiers = Lens.lens (\UpdateRoom' {loggingConfigurationIdentifiers} -> loggingConfigurationIdentifiers) (\s@UpdateRoom' {} a -> s {loggingConfigurationIdentifiers = a} :: UpdateRoom) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of characters in a single message. Messages are
-- expected to be UTF-8 encoded and this limit applies specifically to
-- rune\/code-point count, not number of bytes. Default: 500.
updateRoom_maximumMessageLength :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Natural)
updateRoom_maximumMessageLength = Lens.lens (\UpdateRoom' {maximumMessageLength} -> maximumMessageLength) (\s@UpdateRoom' {} a -> s {maximumMessageLength = a} :: UpdateRoom)

-- | Maximum number of messages per second that can be sent to the room (by
-- all clients). Default: 10.
updateRoom_maximumMessageRatePerSecond :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Natural)
updateRoom_maximumMessageRatePerSecond = Lens.lens (\UpdateRoom' {maximumMessageRatePerSecond} -> maximumMessageRatePerSecond) (\s@UpdateRoom' {} a -> s {maximumMessageRatePerSecond = a} :: UpdateRoom)

-- | Configuration information for optional review of messages. Specify an
-- empty @uri@ string to disassociate a message review handler from the
-- specified room.
updateRoom_messageReviewHandler :: Lens.Lens' UpdateRoom (Prelude.Maybe MessageReviewHandler)
updateRoom_messageReviewHandler = Lens.lens (\UpdateRoom' {messageReviewHandler} -> messageReviewHandler) (\s@UpdateRoom' {} a -> s {messageReviewHandler = a} :: UpdateRoom)

-- | Room name. The value does not need to be unique.
updateRoom_name :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_name = Lens.lens (\UpdateRoom' {name} -> name) (\s@UpdateRoom' {} a -> s {name = a} :: UpdateRoom)

-- | Identifier of the room to be updated. Currently this must be an ARN.
updateRoom_identifier :: Lens.Lens' UpdateRoom Prelude.Text
updateRoom_identifier = Lens.lens (\UpdateRoom' {identifier} -> identifier) (\s@UpdateRoom' {} a -> s {identifier = a} :: UpdateRoom)

instance Core.AWSRequest UpdateRoom where
  type AWSResponse UpdateRoom = UpdateRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoomResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> ( x Data..?> "loggingConfigurationIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "maximumMessageLength")
            Prelude.<*> (x Data..?> "maximumMessageRatePerSecond")
            Prelude.<*> (x Data..?> "messageReviewHandler")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "updateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoom where
  hashWithSalt _salt UpdateRoom' {..} =
    _salt
      `Prelude.hashWithSalt` loggingConfigurationIdentifiers
      `Prelude.hashWithSalt` maximumMessageLength
      `Prelude.hashWithSalt` maximumMessageRatePerSecond
      `Prelude.hashWithSalt` messageReviewHandler
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData UpdateRoom where
  rnf UpdateRoom' {..} =
    Prelude.rnf loggingConfigurationIdentifiers
      `Prelude.seq` Prelude.rnf maximumMessageLength
      `Prelude.seq` Prelude.rnf maximumMessageRatePerSecond
      `Prelude.seq` Prelude.rnf messageReviewHandler
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders UpdateRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoom where
  toJSON UpdateRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("loggingConfigurationIdentifiers" Data..=)
              Prelude.<$> loggingConfigurationIdentifiers,
            ("maximumMessageLength" Data..=)
              Prelude.<$> maximumMessageLength,
            ("maximumMessageRatePerSecond" Data..=)
              Prelude.<$> maximumMessageRatePerSecond,
            ("messageReviewHandler" Data..=)
              Prelude.<$> messageReviewHandler,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("identifier" Data..= identifier)
          ]
      )

instance Data.ToPath UpdateRoom where
  toPath = Prelude.const "/UpdateRoom"

instance Data.ToQuery UpdateRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoomResponse' smart constructor.
data UpdateRoomResponse = UpdateRoomResponse'
  { -- | Room ARN, from the request (if @identifier@ was an ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | Time when the room was created. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | Room ID, generated by the system. This is a relative identifier, the
    -- part of the ARN that uniquely identifies the room.
    id :: Prelude.Maybe Prelude.Text,
    -- | Array of logging configurations attached to the room, from the request
    -- (if specified).
    loggingConfigurationIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of characters in a single message, from the request (if
    -- specified).
    maximumMessageLength :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of messages per second that can be sent to the room (by
    -- all clients), from the request (if specified).
    maximumMessageRatePerSecond :: Prelude.Maybe Prelude.Natural,
    -- | Configuration information for optional review of messages.
    messageReviewHandler :: Prelude.Maybe MessageReviewHandler,
    -- | Room name, from the request (if specified).
    name :: Prelude.Maybe Prelude.Text,
    -- | Tags attached to the resource. Array of maps, each of the form
    -- @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Time of the room’s last update. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    updateTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateRoomResponse_arn' - Room ARN, from the request (if @identifier@ was an ARN).
--
-- 'createTime', 'updateRoomResponse_createTime' - Time when the room was created. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
--
-- 'id', 'updateRoomResponse_id' - Room ID, generated by the system. This is a relative identifier, the
-- part of the ARN that uniquely identifies the room.
--
-- 'loggingConfigurationIdentifiers', 'updateRoomResponse_loggingConfigurationIdentifiers' - Array of logging configurations attached to the room, from the request
-- (if specified).
--
-- 'maximumMessageLength', 'updateRoomResponse_maximumMessageLength' - Maximum number of characters in a single message, from the request (if
-- specified).
--
-- 'maximumMessageRatePerSecond', 'updateRoomResponse_maximumMessageRatePerSecond' - Maximum number of messages per second that can be sent to the room (by
-- all clients), from the request (if specified).
--
-- 'messageReviewHandler', 'updateRoomResponse_messageReviewHandler' - Configuration information for optional review of messages.
--
-- 'name', 'updateRoomResponse_name' - Room name, from the request (if specified).
--
-- 'tags', 'updateRoomResponse_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@.
--
-- 'updateTime', 'updateRoomResponse_updateTime' - Time of the room’s last update. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
--
-- 'httpStatus', 'updateRoomResponse_httpStatus' - The response's http status code.
newUpdateRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoomResponse
newUpdateRoomResponse pHttpStatus_ =
  UpdateRoomResponse'
    { arn = Prelude.Nothing,
      createTime = Prelude.Nothing,
      id = Prelude.Nothing,
      loggingConfigurationIdentifiers = Prelude.Nothing,
      maximumMessageLength = Prelude.Nothing,
      maximumMessageRatePerSecond = Prelude.Nothing,
      messageReviewHandler = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Room ARN, from the request (if @identifier@ was an ARN).
updateRoomResponse_arn :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.Text)
updateRoomResponse_arn = Lens.lens (\UpdateRoomResponse' {arn} -> arn) (\s@UpdateRoomResponse' {} a -> s {arn = a} :: UpdateRoomResponse)

-- | Time when the room was created. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
updateRoomResponse_createTime :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.UTCTime)
updateRoomResponse_createTime = Lens.lens (\UpdateRoomResponse' {createTime} -> createTime) (\s@UpdateRoomResponse' {} a -> s {createTime = a} :: UpdateRoomResponse) Prelude.. Lens.mapping Data._Time

-- | Room ID, generated by the system. This is a relative identifier, the
-- part of the ARN that uniquely identifies the room.
updateRoomResponse_id :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.Text)
updateRoomResponse_id = Lens.lens (\UpdateRoomResponse' {id} -> id) (\s@UpdateRoomResponse' {} a -> s {id = a} :: UpdateRoomResponse)

-- | Array of logging configurations attached to the room, from the request
-- (if specified).
updateRoomResponse_loggingConfigurationIdentifiers :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe [Prelude.Text])
updateRoomResponse_loggingConfigurationIdentifiers = Lens.lens (\UpdateRoomResponse' {loggingConfigurationIdentifiers} -> loggingConfigurationIdentifiers) (\s@UpdateRoomResponse' {} a -> s {loggingConfigurationIdentifiers = a} :: UpdateRoomResponse) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of characters in a single message, from the request (if
-- specified).
updateRoomResponse_maximumMessageLength :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.Natural)
updateRoomResponse_maximumMessageLength = Lens.lens (\UpdateRoomResponse' {maximumMessageLength} -> maximumMessageLength) (\s@UpdateRoomResponse' {} a -> s {maximumMessageLength = a} :: UpdateRoomResponse)

-- | Maximum number of messages per second that can be sent to the room (by
-- all clients), from the request (if specified).
updateRoomResponse_maximumMessageRatePerSecond :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.Natural)
updateRoomResponse_maximumMessageRatePerSecond = Lens.lens (\UpdateRoomResponse' {maximumMessageRatePerSecond} -> maximumMessageRatePerSecond) (\s@UpdateRoomResponse' {} a -> s {maximumMessageRatePerSecond = a} :: UpdateRoomResponse)

-- | Configuration information for optional review of messages.
updateRoomResponse_messageReviewHandler :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe MessageReviewHandler)
updateRoomResponse_messageReviewHandler = Lens.lens (\UpdateRoomResponse' {messageReviewHandler} -> messageReviewHandler) (\s@UpdateRoomResponse' {} a -> s {messageReviewHandler = a} :: UpdateRoomResponse)

-- | Room name, from the request (if specified).
updateRoomResponse_name :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.Text)
updateRoomResponse_name = Lens.lens (\UpdateRoomResponse' {name} -> name) (\s@UpdateRoomResponse' {} a -> s {name = a} :: UpdateRoomResponse)

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@.
updateRoomResponse_tags :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRoomResponse_tags = Lens.lens (\UpdateRoomResponse' {tags} -> tags) (\s@UpdateRoomResponse' {} a -> s {tags = a} :: UpdateRoomResponse) Prelude.. Lens.mapping Lens.coerced

-- | Time of the room’s last update. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
updateRoomResponse_updateTime :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Prelude.UTCTime)
updateRoomResponse_updateTime = Lens.lens (\UpdateRoomResponse' {updateTime} -> updateTime) (\s@UpdateRoomResponse' {} a -> s {updateTime = a} :: UpdateRoomResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateRoomResponse_httpStatus :: Lens.Lens' UpdateRoomResponse Prelude.Int
updateRoomResponse_httpStatus = Lens.lens (\UpdateRoomResponse' {httpStatus} -> httpStatus) (\s@UpdateRoomResponse' {} a -> s {httpStatus = a} :: UpdateRoomResponse)

instance Prelude.NFData UpdateRoomResponse where
  rnf UpdateRoomResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf loggingConfigurationIdentifiers
      `Prelude.seq` Prelude.rnf maximumMessageLength
      `Prelude.seq` Prelude.rnf maximumMessageRatePerSecond
      `Prelude.seq` Prelude.rnf messageReviewHandler
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf httpStatus
