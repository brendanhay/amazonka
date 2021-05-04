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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEventBus' smart constructor.
data CreateEventBus = CreateEventBus'
  { -- | Tags to associate with the event bus.
    tags :: Prelude.Maybe [Tag],
    -- | If you are creating a partner event bus, this specifies the partner
    -- event source that the new event bus will be matched with.
    eventSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the new event bus.
    --
    -- Event bus names cannot contain the \/ character. You can\'t use the name
    -- @default@ for a custom event bus, as this name is already used for your
    -- account\'s default event bus.
    --
    -- If this is a partner event bus, the name must exactly match the name of
    -- the partner event source that this event bus is matched to.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateEventBus
newCreateEventBus pName_ =
  CreateEventBus'
    { tags = Prelude.Nothing,
      eventSourceName = Prelude.Nothing,
      name = pName_
    }

-- | Tags to associate with the event bus.
createEventBus_tags :: Lens.Lens' CreateEventBus (Prelude.Maybe [Tag])
createEventBus_tags = Lens.lens (\CreateEventBus' {tags} -> tags) (\s@CreateEventBus' {} a -> s {tags = a} :: CreateEventBus) Prelude.. Lens.mapping Prelude._Coerce

-- | If you are creating a partner event bus, this specifies the partner
-- event source that the new event bus will be matched with.
createEventBus_eventSourceName :: Lens.Lens' CreateEventBus (Prelude.Maybe Prelude.Text)
createEventBus_eventSourceName = Lens.lens (\CreateEventBus' {eventSourceName} -> eventSourceName) (\s@CreateEventBus' {} a -> s {eventSourceName = a} :: CreateEventBus)

-- | The name of the new event bus.
--
-- Event bus names cannot contain the \/ character. You can\'t use the name
-- @default@ for a custom event bus, as this name is already used for your
-- account\'s default event bus.
--
-- If this is a partner event bus, the name must exactly match the name of
-- the partner event source that this event bus is matched to.
createEventBus_name :: Lens.Lens' CreateEventBus Prelude.Text
createEventBus_name = Lens.lens (\CreateEventBus' {name} -> name) (\s@CreateEventBus' {} a -> s {name = a} :: CreateEventBus)

instance Prelude.AWSRequest CreateEventBus where
  type Rs CreateEventBus = CreateEventBusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventBusResponse'
            Prelude.<$> (x Prelude..?> "EventBusArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventBus

instance Prelude.NFData CreateEventBus

instance Prelude.ToHeaders CreateEventBus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.CreateEventBus" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateEventBus where
  toJSON CreateEventBus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("EventSourceName" Prelude..=)
              Prelude.<$> eventSourceName,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateEventBus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateEventBus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventBusResponse' smart constructor.
data CreateEventBusResponse = CreateEventBusResponse'
  { -- | The ARN of the new event bus.
    eventBusArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateEventBusResponse
newCreateEventBusResponse pHttpStatus_ =
  CreateEventBusResponse'
    { eventBusArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new event bus.
createEventBusResponse_eventBusArn :: Lens.Lens' CreateEventBusResponse (Prelude.Maybe Prelude.Text)
createEventBusResponse_eventBusArn = Lens.lens (\CreateEventBusResponse' {eventBusArn} -> eventBusArn) (\s@CreateEventBusResponse' {} a -> s {eventBusArn = a} :: CreateEventBusResponse)

-- | The response's http status code.
createEventBusResponse_httpStatus :: Lens.Lens' CreateEventBusResponse Prelude.Int
createEventBusResponse_httpStatus = Lens.lens (\CreateEventBusResponse' {httpStatus} -> httpStatus) (\s@CreateEventBusResponse' {} a -> s {httpStatus = a} :: CreateEventBusResponse)

instance Prelude.NFData CreateEventBusResponse
