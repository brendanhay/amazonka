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
-- Module      : Amazonka.IVSChat.CreateRoom
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room that allows clients to connect and pass messages.
module Amazonka.IVSChat.CreateRoom
  ( -- * Creating a Request
    CreateRoom (..),
    newCreateRoom,

    -- * Request Lenses
    createRoom_tags,
    createRoom_loggingConfigurationIdentifiers,
    createRoom_name,
    createRoom_messageReviewHandler,
    createRoom_maximumMessageRatePerSecond,
    createRoom_maximumMessageLength,

    -- * Destructuring the Response
    CreateRoomResponse (..),
    newCreateRoomResponse,

    -- * Response Lenses
    createRoomResponse_tags,
    createRoomResponse_loggingConfigurationIdentifiers,
    createRoomResponse_name,
    createRoomResponse_messageReviewHandler,
    createRoomResponse_arn,
    createRoomResponse_id,
    createRoomResponse_maximumMessageRatePerSecond,
    createRoomResponse_maximumMessageLength,
    createRoomResponse_updateTime,
    createRoomResponse_createTime,
    createRoomResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRoom' smart constructor.
data CreateRoom = CreateRoom'
  { -- | Tags to attach to the resource. Array of maps, each of the form
    -- @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- for details, including restrictions that apply to tags and \"Tag naming
    -- limits and requirements\"; Amazon IVS Chat has no constraints beyond
    -- what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Array of logging-configuration identifiers attached to the room.
    loggingConfigurationIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Room name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for optional review of messages.
    messageReviewHandler :: Prelude.Maybe MessageReviewHandler,
    -- | Maximum number of messages per second that can be sent to the room (by
    -- all clients). Default: 10.
    maximumMessageRatePerSecond :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of characters in a single message. Messages are expected
    -- to be UTF-8 encoded and this limit applies specifically to
    -- rune\/code-point count, not number of bytes. Default: 500.
    maximumMessageLength :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRoom_tags' - Tags to attach to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS Chat has no constraints beyond
-- what is documented there.
--
-- 'loggingConfigurationIdentifiers', 'createRoom_loggingConfigurationIdentifiers' - Array of logging-configuration identifiers attached to the room.
--
-- 'name', 'createRoom_name' - Room name. The value does not need to be unique.
--
-- 'messageReviewHandler', 'createRoom_messageReviewHandler' - Configuration information for optional review of messages.
--
-- 'maximumMessageRatePerSecond', 'createRoom_maximumMessageRatePerSecond' - Maximum number of messages per second that can be sent to the room (by
-- all clients). Default: 10.
--
-- 'maximumMessageLength', 'createRoom_maximumMessageLength' - Maximum number of characters in a single message. Messages are expected
-- to be UTF-8 encoded and this limit applies specifically to
-- rune\/code-point count, not number of bytes. Default: 500.
newCreateRoom ::
  CreateRoom
newCreateRoom =
  CreateRoom'
    { tags = Prelude.Nothing,
      loggingConfigurationIdentifiers = Prelude.Nothing,
      name = Prelude.Nothing,
      messageReviewHandler = Prelude.Nothing,
      maximumMessageRatePerSecond = Prelude.Nothing,
      maximumMessageLength = Prelude.Nothing
    }

-- | Tags to attach to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS Chat has no constraints beyond
-- what is documented there.
createRoom_tags :: Lens.Lens' CreateRoom (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRoom_tags = Lens.lens (\CreateRoom' {tags} -> tags) (\s@CreateRoom' {} a -> s {tags = a} :: CreateRoom) Prelude.. Lens.mapping Lens.coerced

-- | Array of logging-configuration identifiers attached to the room.
createRoom_loggingConfigurationIdentifiers :: Lens.Lens' CreateRoom (Prelude.Maybe [Prelude.Text])
createRoom_loggingConfigurationIdentifiers = Lens.lens (\CreateRoom' {loggingConfigurationIdentifiers} -> loggingConfigurationIdentifiers) (\s@CreateRoom' {} a -> s {loggingConfigurationIdentifiers = a} :: CreateRoom) Prelude.. Lens.mapping Lens.coerced

-- | Room name. The value does not need to be unique.
createRoom_name :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Text)
createRoom_name = Lens.lens (\CreateRoom' {name} -> name) (\s@CreateRoom' {} a -> s {name = a} :: CreateRoom)

-- | Configuration information for optional review of messages.
createRoom_messageReviewHandler :: Lens.Lens' CreateRoom (Prelude.Maybe MessageReviewHandler)
createRoom_messageReviewHandler = Lens.lens (\CreateRoom' {messageReviewHandler} -> messageReviewHandler) (\s@CreateRoom' {} a -> s {messageReviewHandler = a} :: CreateRoom)

-- | Maximum number of messages per second that can be sent to the room (by
-- all clients). Default: 10.
createRoom_maximumMessageRatePerSecond :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Natural)
createRoom_maximumMessageRatePerSecond = Lens.lens (\CreateRoom' {maximumMessageRatePerSecond} -> maximumMessageRatePerSecond) (\s@CreateRoom' {} a -> s {maximumMessageRatePerSecond = a} :: CreateRoom)

-- | Maximum number of characters in a single message. Messages are expected
-- to be UTF-8 encoded and this limit applies specifically to
-- rune\/code-point count, not number of bytes. Default: 500.
createRoom_maximumMessageLength :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Natural)
createRoom_maximumMessageLength = Lens.lens (\CreateRoom' {maximumMessageLength} -> maximumMessageLength) (\s@CreateRoom' {} a -> s {maximumMessageLength = a} :: CreateRoom)

instance Core.AWSRequest CreateRoom where
  type AWSResponse CreateRoom = CreateRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoomResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "loggingConfigurationIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "messageReviewHandler")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "maximumMessageRatePerSecond")
            Prelude.<*> (x Core..?> "maximumMessageLength")
            Prelude.<*> (x Core..?> "updateTime")
            Prelude.<*> (x Core..?> "createTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoom where
  hashWithSalt _salt CreateRoom' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` loggingConfigurationIdentifiers
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` messageReviewHandler
      `Prelude.hashWithSalt` maximumMessageRatePerSecond
      `Prelude.hashWithSalt` maximumMessageLength

instance Prelude.NFData CreateRoom where
  rnf CreateRoom' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf loggingConfigurationIdentifiers
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf messageReviewHandler
      `Prelude.seq` Prelude.rnf maximumMessageRatePerSecond
      `Prelude.seq` Prelude.rnf maximumMessageLength

instance Core.ToHeaders CreateRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRoom where
  toJSON CreateRoom' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("loggingConfigurationIdentifiers" Core..=)
              Prelude.<$> loggingConfigurationIdentifiers,
            ("name" Core..=) Prelude.<$> name,
            ("messageReviewHandler" Core..=)
              Prelude.<$> messageReviewHandler,
            ("maximumMessageRatePerSecond" Core..=)
              Prelude.<$> maximumMessageRatePerSecond,
            ("maximumMessageLength" Core..=)
              Prelude.<$> maximumMessageLength
          ]
      )

instance Core.ToPath CreateRoom where
  toPath = Prelude.const "/CreateRoom"

instance Core.ToQuery CreateRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { -- | Tags attached to the resource, from the request (if specified).
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Array of logging configurations attached to the room, from the request
    -- (if specified).
    loggingConfigurationIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Room name, from the request (if specified).
    name :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for optional review of messages.
    messageReviewHandler :: Prelude.Maybe MessageReviewHandler,
    -- | Room ARN, assigned by the system.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Room ID, generated by the system. This is a relative identifier, the
    -- part of the ARN that uniquely identifies the room.
    id :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of messages per second that can be sent to the room (by
    -- all clients), from the request (if specified).
    maximumMessageRatePerSecond :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of characters in a single message, from the request (if
    -- specified).
    maximumMessageLength :: Prelude.Maybe Prelude.Natural,
    -- | Time of the room’s last update. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    updateTime :: Prelude.Maybe Core.POSIX,
    -- | Time when the room was created. This is an ISO 8601 timestamp; /note
    -- that this is returned as a string/.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRoomResponse_tags' - Tags attached to the resource, from the request (if specified).
--
-- 'loggingConfigurationIdentifiers', 'createRoomResponse_loggingConfigurationIdentifiers' - Array of logging configurations attached to the room, from the request
-- (if specified).
--
-- 'name', 'createRoomResponse_name' - Room name, from the request (if specified).
--
-- 'messageReviewHandler', 'createRoomResponse_messageReviewHandler' - Configuration information for optional review of messages.
--
-- 'arn', 'createRoomResponse_arn' - Room ARN, assigned by the system.
--
-- 'id', 'createRoomResponse_id' - Room ID, generated by the system. This is a relative identifier, the
-- part of the ARN that uniquely identifies the room.
--
-- 'maximumMessageRatePerSecond', 'createRoomResponse_maximumMessageRatePerSecond' - Maximum number of messages per second that can be sent to the room (by
-- all clients), from the request (if specified).
--
-- 'maximumMessageLength', 'createRoomResponse_maximumMessageLength' - Maximum number of characters in a single message, from the request (if
-- specified).
--
-- 'updateTime', 'createRoomResponse_updateTime' - Time of the room’s last update. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
--
-- 'createTime', 'createRoomResponse_createTime' - Time when the room was created. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
--
-- 'httpStatus', 'createRoomResponse_httpStatus' - The response's http status code.
newCreateRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRoomResponse
newCreateRoomResponse pHttpStatus_ =
  CreateRoomResponse'
    { tags = Prelude.Nothing,
      loggingConfigurationIdentifiers = Prelude.Nothing,
      name = Prelude.Nothing,
      messageReviewHandler = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      maximumMessageRatePerSecond = Prelude.Nothing,
      maximumMessageLength = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tags attached to the resource, from the request (if specified).
createRoomResponse_tags :: Lens.Lens' CreateRoomResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRoomResponse_tags = Lens.lens (\CreateRoomResponse' {tags} -> tags) (\s@CreateRoomResponse' {} a -> s {tags = a} :: CreateRoomResponse) Prelude.. Lens.mapping Lens.coerced

-- | Array of logging configurations attached to the room, from the request
-- (if specified).
createRoomResponse_loggingConfigurationIdentifiers :: Lens.Lens' CreateRoomResponse (Prelude.Maybe [Prelude.Text])
createRoomResponse_loggingConfigurationIdentifiers = Lens.lens (\CreateRoomResponse' {loggingConfigurationIdentifiers} -> loggingConfigurationIdentifiers) (\s@CreateRoomResponse' {} a -> s {loggingConfigurationIdentifiers = a} :: CreateRoomResponse) Prelude.. Lens.mapping Lens.coerced

-- | Room name, from the request (if specified).
createRoomResponse_name :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.Text)
createRoomResponse_name = Lens.lens (\CreateRoomResponse' {name} -> name) (\s@CreateRoomResponse' {} a -> s {name = a} :: CreateRoomResponse)

-- | Configuration information for optional review of messages.
createRoomResponse_messageReviewHandler :: Lens.Lens' CreateRoomResponse (Prelude.Maybe MessageReviewHandler)
createRoomResponse_messageReviewHandler = Lens.lens (\CreateRoomResponse' {messageReviewHandler} -> messageReviewHandler) (\s@CreateRoomResponse' {} a -> s {messageReviewHandler = a} :: CreateRoomResponse)

-- | Room ARN, assigned by the system.
createRoomResponse_arn :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.Text)
createRoomResponse_arn = Lens.lens (\CreateRoomResponse' {arn} -> arn) (\s@CreateRoomResponse' {} a -> s {arn = a} :: CreateRoomResponse)

-- | Room ID, generated by the system. This is a relative identifier, the
-- part of the ARN that uniquely identifies the room.
createRoomResponse_id :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.Text)
createRoomResponse_id = Lens.lens (\CreateRoomResponse' {id} -> id) (\s@CreateRoomResponse' {} a -> s {id = a} :: CreateRoomResponse)

-- | Maximum number of messages per second that can be sent to the room (by
-- all clients), from the request (if specified).
createRoomResponse_maximumMessageRatePerSecond :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.Natural)
createRoomResponse_maximumMessageRatePerSecond = Lens.lens (\CreateRoomResponse' {maximumMessageRatePerSecond} -> maximumMessageRatePerSecond) (\s@CreateRoomResponse' {} a -> s {maximumMessageRatePerSecond = a} :: CreateRoomResponse)

-- | Maximum number of characters in a single message, from the request (if
-- specified).
createRoomResponse_maximumMessageLength :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.Natural)
createRoomResponse_maximumMessageLength = Lens.lens (\CreateRoomResponse' {maximumMessageLength} -> maximumMessageLength) (\s@CreateRoomResponse' {} a -> s {maximumMessageLength = a} :: CreateRoomResponse)

-- | Time of the room’s last update. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
createRoomResponse_updateTime :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.UTCTime)
createRoomResponse_updateTime = Lens.lens (\CreateRoomResponse' {updateTime} -> updateTime) (\s@CreateRoomResponse' {} a -> s {updateTime = a} :: CreateRoomResponse) Prelude.. Lens.mapping Core._Time

-- | Time when the room was created. This is an ISO 8601 timestamp; /note
-- that this is returned as a string/.
createRoomResponse_createTime :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.UTCTime)
createRoomResponse_createTime = Lens.lens (\CreateRoomResponse' {createTime} -> createTime) (\s@CreateRoomResponse' {} a -> s {createTime = a} :: CreateRoomResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
createRoomResponse_httpStatus :: Lens.Lens' CreateRoomResponse Prelude.Int
createRoomResponse_httpStatus = Lens.lens (\CreateRoomResponse' {httpStatus} -> httpStatus) (\s@CreateRoomResponse' {} a -> s {httpStatus = a} :: CreateRoomResponse)

instance Prelude.NFData CreateRoomResponse where
  rnf CreateRoomResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf loggingConfigurationIdentifiers
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf messageReviewHandler
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf maximumMessageRatePerSecond
      `Prelude.seq` Prelude.rnf maximumMessageLength
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf httpStatus
