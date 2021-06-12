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
-- Module      : Network.AWS.CloudWatchEvents.CreateEventBus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event bus within your account. This can be a custom event
-- bus which you can use to receive events from your custom applications
-- and services, or it can be a partner event bus which can be matched to a
-- partner event source.
module Network.AWS.CloudWatchEvents.CreateEventBus
  ( -- * Creating a Request
    CreateEventBus (..),
    newCreateEventBus,

    -- * Request Lenses
    createEventBus_tags,
    createEventBus_eventSourceName,
    createEventBus_name,

    -- * Destructuring the Response
    CreateEventBusResponse (..),
    newCreateEventBusResponse,

    -- * Response Lenses
    createEventBusResponse_eventBusArn,
    createEventBusResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEventBus' smart constructor.
data CreateEventBus = CreateEventBus'
  { -- | Tags to associate with the event bus.
    tags :: Core.Maybe [Tag],
    -- | If you are creating a partner event bus, this specifies the partner
    -- event source that the new event bus will be matched with.
    eventSourceName :: Core.Maybe Core.Text,
    -- | The name of the new event bus.
    --
    -- Event bus names cannot contain the \/ character. You can\'t use the name
    -- @default@ for a custom event bus, as this name is already used for your
    -- account\'s default event bus.
    --
    -- If this is a partner event bus, the name must exactly match the name of
    -- the partner event source that this event bus is matched to.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateEventBus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEventBus_tags' - Tags to associate with the event bus.
--
-- 'eventSourceName', 'createEventBus_eventSourceName' - If you are creating a partner event bus, this specifies the partner
-- event source that the new event bus will be matched with.
--
-- 'name', 'createEventBus_name' - The name of the new event bus.
--
-- Event bus names cannot contain the \/ character. You can\'t use the name
-- @default@ for a custom event bus, as this name is already used for your
-- account\'s default event bus.
--
-- If this is a partner event bus, the name must exactly match the name of
-- the partner event source that this event bus is matched to.
newCreateEventBus ::
  -- | 'name'
  Core.Text ->
  CreateEventBus
newCreateEventBus pName_ =
  CreateEventBus'
    { tags = Core.Nothing,
      eventSourceName = Core.Nothing,
      name = pName_
    }

-- | Tags to associate with the event bus.
createEventBus_tags :: Lens.Lens' CreateEventBus (Core.Maybe [Tag])
createEventBus_tags = Lens.lens (\CreateEventBus' {tags} -> tags) (\s@CreateEventBus' {} a -> s {tags = a} :: CreateEventBus) Core.. Lens.mapping Lens._Coerce

-- | If you are creating a partner event bus, this specifies the partner
-- event source that the new event bus will be matched with.
createEventBus_eventSourceName :: Lens.Lens' CreateEventBus (Core.Maybe Core.Text)
createEventBus_eventSourceName = Lens.lens (\CreateEventBus' {eventSourceName} -> eventSourceName) (\s@CreateEventBus' {} a -> s {eventSourceName = a} :: CreateEventBus)

-- | The name of the new event bus.
--
-- Event bus names cannot contain the \/ character. You can\'t use the name
-- @default@ for a custom event bus, as this name is already used for your
-- account\'s default event bus.
--
-- If this is a partner event bus, the name must exactly match the name of
-- the partner event source that this event bus is matched to.
createEventBus_name :: Lens.Lens' CreateEventBus Core.Text
createEventBus_name = Lens.lens (\CreateEventBus' {name} -> name) (\s@CreateEventBus' {} a -> s {name = a} :: CreateEventBus)

instance Core.AWSRequest CreateEventBus where
  type
    AWSResponse CreateEventBus =
      CreateEventBusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventBusResponse'
            Core.<$> (x Core..?> "EventBusArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateEventBus

instance Core.NFData CreateEventBus

instance Core.ToHeaders CreateEventBus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.CreateEventBus" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateEventBus where
  toJSON CreateEventBus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("EventSourceName" Core..=) Core.<$> eventSourceName,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateEventBus where
  toPath = Core.const "/"

instance Core.ToQuery CreateEventBus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateEventBusResponse' smart constructor.
data CreateEventBusResponse = CreateEventBusResponse'
  { -- | The ARN of the new event bus.
    eventBusArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateEventBusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusArn', 'createEventBusResponse_eventBusArn' - The ARN of the new event bus.
--
-- 'httpStatus', 'createEventBusResponse_httpStatus' - The response's http status code.
newCreateEventBusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateEventBusResponse
newCreateEventBusResponse pHttpStatus_ =
  CreateEventBusResponse'
    { eventBusArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new event bus.
createEventBusResponse_eventBusArn :: Lens.Lens' CreateEventBusResponse (Core.Maybe Core.Text)
createEventBusResponse_eventBusArn = Lens.lens (\CreateEventBusResponse' {eventBusArn} -> eventBusArn) (\s@CreateEventBusResponse' {} a -> s {eventBusArn = a} :: CreateEventBusResponse)

-- | The response's http status code.
createEventBusResponse_httpStatus :: Lens.Lens' CreateEventBusResponse Core.Int
createEventBusResponse_httpStatus = Lens.lens (\CreateEventBusResponse' {httpStatus} -> httpStatus) (\s@CreateEventBusResponse' {} a -> s {httpStatus = a} :: CreateEventBusResponse)

instance Core.NFData CreateEventBusResponse
