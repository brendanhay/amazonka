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
-- Module      : Amazonka.Connect.CreateAgentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Creates an agent status for the specified Amazon Connect instance.
module Amazonka.Connect.CreateAgentStatus
  ( -- * Creating a Request
    CreateAgentStatus (..),
    newCreateAgentStatus,

    -- * Request Lenses
    createAgentStatus_description,
    createAgentStatus_displayOrder,
    createAgentStatus_tags,
    createAgentStatus_instanceId,
    createAgentStatus_name,
    createAgentStatus_state,

    -- * Destructuring the Response
    CreateAgentStatusResponse (..),
    newCreateAgentStatusResponse,

    -- * Response Lenses
    createAgentStatusResponse_agentStatusARN,
    createAgentStatusResponse_agentStatusId,
    createAgentStatusResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAgentStatus' smart constructor.
data CreateAgentStatus = CreateAgentStatus'
  { -- | The description of the status.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display order of the status.
    displayOrder :: Prelude.Maybe Prelude.Natural,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the status.
    name :: Prelude.Text,
    -- | The state of the status.
    state :: AgentStatusState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAgentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAgentStatus_description' - The description of the status.
--
-- 'displayOrder', 'createAgentStatus_displayOrder' - The display order of the status.
--
-- 'tags', 'createAgentStatus_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'instanceId', 'createAgentStatus_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'createAgentStatus_name' - The name of the status.
--
-- 'state', 'createAgentStatus_state' - The state of the status.
newCreateAgentStatus ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'state'
  AgentStatusState ->
  CreateAgentStatus
newCreateAgentStatus pInstanceId_ pName_ pState_ =
  CreateAgentStatus'
    { description = Prelude.Nothing,
      displayOrder = Prelude.Nothing,
      tags = Prelude.Nothing,
      instanceId = pInstanceId_,
      name = pName_,
      state = pState_
    }

-- | The description of the status.
createAgentStatus_description :: Lens.Lens' CreateAgentStatus (Prelude.Maybe Prelude.Text)
createAgentStatus_description = Lens.lens (\CreateAgentStatus' {description} -> description) (\s@CreateAgentStatus' {} a -> s {description = a} :: CreateAgentStatus)

-- | The display order of the status.
createAgentStatus_displayOrder :: Lens.Lens' CreateAgentStatus (Prelude.Maybe Prelude.Natural)
createAgentStatus_displayOrder = Lens.lens (\CreateAgentStatus' {displayOrder} -> displayOrder) (\s@CreateAgentStatus' {} a -> s {displayOrder = a} :: CreateAgentStatus)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createAgentStatus_tags :: Lens.Lens' CreateAgentStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAgentStatus_tags = Lens.lens (\CreateAgentStatus' {tags} -> tags) (\s@CreateAgentStatus' {} a -> s {tags = a} :: CreateAgentStatus) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createAgentStatus_instanceId :: Lens.Lens' CreateAgentStatus Prelude.Text
createAgentStatus_instanceId = Lens.lens (\CreateAgentStatus' {instanceId} -> instanceId) (\s@CreateAgentStatus' {} a -> s {instanceId = a} :: CreateAgentStatus)

-- | The name of the status.
createAgentStatus_name :: Lens.Lens' CreateAgentStatus Prelude.Text
createAgentStatus_name = Lens.lens (\CreateAgentStatus' {name} -> name) (\s@CreateAgentStatus' {} a -> s {name = a} :: CreateAgentStatus)

-- | The state of the status.
createAgentStatus_state :: Lens.Lens' CreateAgentStatus AgentStatusState
createAgentStatus_state = Lens.lens (\CreateAgentStatus' {state} -> state) (\s@CreateAgentStatus' {} a -> s {state = a} :: CreateAgentStatus)

instance Core.AWSRequest CreateAgentStatus where
  type
    AWSResponse CreateAgentStatus =
      CreateAgentStatusResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAgentStatusResponse'
            Prelude.<$> (x Data..?> "AgentStatusARN")
            Prelude.<*> (x Data..?> "AgentStatusId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAgentStatus where
  hashWithSalt _salt CreateAgentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayOrder
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData CreateAgentStatus where
  rnf CreateAgentStatus' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayOrder
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state

instance Data.ToHeaders CreateAgentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAgentStatus where
  toJSON CreateAgentStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisplayOrder" Data..=) Prelude.<$> displayOrder,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("State" Data..= state)
          ]
      )

instance Data.ToPath CreateAgentStatus where
  toPath CreateAgentStatus' {..} =
    Prelude.mconcat
      ["/agent-status/", Data.toBS instanceId]

instance Data.ToQuery CreateAgentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAgentStatusResponse' smart constructor.
data CreateAgentStatusResponse = CreateAgentStatusResponse'
  { -- | The Amazon Resource Name (ARN) of the agent status.
    agentStatusARN :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the agent status.
    agentStatusId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAgentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentStatusARN', 'createAgentStatusResponse_agentStatusARN' - The Amazon Resource Name (ARN) of the agent status.
--
-- 'agentStatusId', 'createAgentStatusResponse_agentStatusId' - The identifier of the agent status.
--
-- 'httpStatus', 'createAgentStatusResponse_httpStatus' - The response's http status code.
newCreateAgentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAgentStatusResponse
newCreateAgentStatusResponse pHttpStatus_ =
  CreateAgentStatusResponse'
    { agentStatusARN =
        Prelude.Nothing,
      agentStatusId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the agent status.
createAgentStatusResponse_agentStatusARN :: Lens.Lens' CreateAgentStatusResponse (Prelude.Maybe Prelude.Text)
createAgentStatusResponse_agentStatusARN = Lens.lens (\CreateAgentStatusResponse' {agentStatusARN} -> agentStatusARN) (\s@CreateAgentStatusResponse' {} a -> s {agentStatusARN = a} :: CreateAgentStatusResponse)

-- | The identifier of the agent status.
createAgentStatusResponse_agentStatusId :: Lens.Lens' CreateAgentStatusResponse (Prelude.Maybe Prelude.Text)
createAgentStatusResponse_agentStatusId = Lens.lens (\CreateAgentStatusResponse' {agentStatusId} -> agentStatusId) (\s@CreateAgentStatusResponse' {} a -> s {agentStatusId = a} :: CreateAgentStatusResponse)

-- | The response's http status code.
createAgentStatusResponse_httpStatus :: Lens.Lens' CreateAgentStatusResponse Prelude.Int
createAgentStatusResponse_httpStatus = Lens.lens (\CreateAgentStatusResponse' {httpStatus} -> httpStatus) (\s@CreateAgentStatusResponse' {} a -> s {httpStatus = a} :: CreateAgentStatusResponse)

instance Prelude.NFData CreateAgentStatusResponse where
  rnf CreateAgentStatusResponse' {..} =
    Prelude.rnf agentStatusARN
      `Prelude.seq` Prelude.rnf agentStatusId
      `Prelude.seq` Prelude.rnf httpStatus
