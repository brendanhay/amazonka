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
-- Module      : Amazonka.AppIntegrationS.CreateEventIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EventIntegration, given a specified name, description, and a
-- reference to an Amazon EventBridge bus in your account and a partner
-- event source that pushes events to that bus. No objects are created in
-- the your account, only metadata that is persisted on the
-- EventIntegration control plane.
module Amazonka.AppIntegrationS.CreateEventIntegration
  ( -- * Creating a Request
    CreateEventIntegration (..),
    newCreateEventIntegration,

    -- * Request Lenses
    createEventIntegration_clientToken,
    createEventIntegration_description,
    createEventIntegration_tags,
    createEventIntegration_name,
    createEventIntegration_eventFilter,
    createEventIntegration_eventBridgeBus,

    -- * Destructuring the Response
    CreateEventIntegrationResponse (..),
    newCreateEventIntegrationResponse,

    -- * Response Lenses
    createEventIntegrationResponse_eventIntegrationArn,
    createEventIntegrationResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEventIntegration' smart constructor.
data CreateEventIntegration = CreateEventIntegration'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the event integration.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the event integration.
    name :: Prelude.Text,
    -- | The event filter.
    eventFilter :: EventFilter,
    -- | The EventBridge bus.
    eventBridgeBus :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createEventIntegration_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createEventIntegration_description' - The description of the event integration.
--
-- 'tags', 'createEventIntegration_tags' - One or more tags.
--
-- 'name', 'createEventIntegration_name' - The name of the event integration.
--
-- 'eventFilter', 'createEventIntegration_eventFilter' - The event filter.
--
-- 'eventBridgeBus', 'createEventIntegration_eventBridgeBus' - The EventBridge bus.
newCreateEventIntegration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'eventFilter'
  EventFilter ->
  -- | 'eventBridgeBus'
  Prelude.Text ->
  CreateEventIntegration
newCreateEventIntegration
  pName_
  pEventFilter_
  pEventBridgeBus_ =
    CreateEventIntegration'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        eventFilter = pEventFilter_,
        eventBridgeBus = pEventBridgeBus_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createEventIntegration_clientToken :: Lens.Lens' CreateEventIntegration (Prelude.Maybe Prelude.Text)
createEventIntegration_clientToken = Lens.lens (\CreateEventIntegration' {clientToken} -> clientToken) (\s@CreateEventIntegration' {} a -> s {clientToken = a} :: CreateEventIntegration)

-- | The description of the event integration.
createEventIntegration_description :: Lens.Lens' CreateEventIntegration (Prelude.Maybe Prelude.Text)
createEventIntegration_description = Lens.lens (\CreateEventIntegration' {description} -> description) (\s@CreateEventIntegration' {} a -> s {description = a} :: CreateEventIntegration)

-- | One or more tags.
createEventIntegration_tags :: Lens.Lens' CreateEventIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEventIntegration_tags = Lens.lens (\CreateEventIntegration' {tags} -> tags) (\s@CreateEventIntegration' {} a -> s {tags = a} :: CreateEventIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the event integration.
createEventIntegration_name :: Lens.Lens' CreateEventIntegration Prelude.Text
createEventIntegration_name = Lens.lens (\CreateEventIntegration' {name} -> name) (\s@CreateEventIntegration' {} a -> s {name = a} :: CreateEventIntegration)

-- | The event filter.
createEventIntegration_eventFilter :: Lens.Lens' CreateEventIntegration EventFilter
createEventIntegration_eventFilter = Lens.lens (\CreateEventIntegration' {eventFilter} -> eventFilter) (\s@CreateEventIntegration' {} a -> s {eventFilter = a} :: CreateEventIntegration)

-- | The EventBridge bus.
createEventIntegration_eventBridgeBus :: Lens.Lens' CreateEventIntegration Prelude.Text
createEventIntegration_eventBridgeBus = Lens.lens (\CreateEventIntegration' {eventBridgeBus} -> eventBridgeBus) (\s@CreateEventIntegration' {} a -> s {eventBridgeBus = a} :: CreateEventIntegration)

instance Core.AWSRequest CreateEventIntegration where
  type
    AWSResponse CreateEventIntegration =
      CreateEventIntegrationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventIntegrationResponse'
            Prelude.<$> (x Data..?> "EventIntegrationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventIntegration where
  hashWithSalt _salt CreateEventIntegration' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` eventFilter
      `Prelude.hashWithSalt` eventBridgeBus

instance Prelude.NFData CreateEventIntegration where
  rnf CreateEventIntegration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventFilter
      `Prelude.seq` Prelude.rnf eventBridgeBus

instance Data.ToHeaders CreateEventIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEventIntegration where
  toJSON CreateEventIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("EventFilter" Data..= eventFilter),
            Prelude.Just
              ("EventBridgeBus" Data..= eventBridgeBus)
          ]
      )

instance Data.ToPath CreateEventIntegration where
  toPath = Prelude.const "/eventIntegrations"

instance Data.ToQuery CreateEventIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventIntegrationResponse' smart constructor.
data CreateEventIntegrationResponse = CreateEventIntegrationResponse'
  { -- | The Amazon Resource Name (ARN) of the event integration.
    eventIntegrationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventIntegrationArn', 'createEventIntegrationResponse_eventIntegrationArn' - The Amazon Resource Name (ARN) of the event integration.
--
-- 'httpStatus', 'createEventIntegrationResponse_httpStatus' - The response's http status code.
newCreateEventIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEventIntegrationResponse
newCreateEventIntegrationResponse pHttpStatus_ =
  CreateEventIntegrationResponse'
    { eventIntegrationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the event integration.
createEventIntegrationResponse_eventIntegrationArn :: Lens.Lens' CreateEventIntegrationResponse (Prelude.Maybe Prelude.Text)
createEventIntegrationResponse_eventIntegrationArn = Lens.lens (\CreateEventIntegrationResponse' {eventIntegrationArn} -> eventIntegrationArn) (\s@CreateEventIntegrationResponse' {} a -> s {eventIntegrationArn = a} :: CreateEventIntegrationResponse)

-- | The response's http status code.
createEventIntegrationResponse_httpStatus :: Lens.Lens' CreateEventIntegrationResponse Prelude.Int
createEventIntegrationResponse_httpStatus = Lens.lens (\CreateEventIntegrationResponse' {httpStatus} -> httpStatus) (\s@CreateEventIntegrationResponse' {} a -> s {httpStatus = a} :: CreateEventIntegrationResponse)

instance
  Prelude.NFData
    CreateEventIntegrationResponse
  where
  rnf CreateEventIntegrationResponse' {..} =
    Prelude.rnf eventIntegrationArn
      `Prelude.seq` Prelude.rnf httpStatus
