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
-- Module      : Amazonka.GroundStation.GetAgentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For use by AWS Ground Station Agent and shouldn\'t be called directly.
--
-- Gets the latest configuration information for a registered agent.
module Amazonka.GroundStation.GetAgentConfiguration
  ( -- * Creating a Request
    GetAgentConfiguration (..),
    newGetAgentConfiguration,

    -- * Request Lenses
    getAgentConfiguration_agentId,

    -- * Destructuring the Response
    GetAgentConfigurationResponse (..),
    newGetAgentConfigurationResponse,

    -- * Response Lenses
    getAgentConfigurationResponse_agentId,
    getAgentConfigurationResponse_taskingDocument,
    getAgentConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAgentConfiguration' smart constructor.
data GetAgentConfiguration = GetAgentConfiguration'
  { -- | UUID of agent to get configuration information for.
    agentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAgentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentId', 'getAgentConfiguration_agentId' - UUID of agent to get configuration information for.
newGetAgentConfiguration ::
  -- | 'agentId'
  Prelude.Text ->
  GetAgentConfiguration
newGetAgentConfiguration pAgentId_ =
  GetAgentConfiguration' {agentId = pAgentId_}

-- | UUID of agent to get configuration information for.
getAgentConfiguration_agentId :: Lens.Lens' GetAgentConfiguration Prelude.Text
getAgentConfiguration_agentId = Lens.lens (\GetAgentConfiguration' {agentId} -> agentId) (\s@GetAgentConfiguration' {} a -> s {agentId = a} :: GetAgentConfiguration)

instance Core.AWSRequest GetAgentConfiguration where
  type
    AWSResponse GetAgentConfiguration =
      GetAgentConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAgentConfigurationResponse'
            Prelude.<$> (x Data..?> "agentId")
            Prelude.<*> (x Data..?> "taskingDocument")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAgentConfiguration where
  hashWithSalt _salt GetAgentConfiguration' {..} =
    _salt `Prelude.hashWithSalt` agentId

instance Prelude.NFData GetAgentConfiguration where
  rnf GetAgentConfiguration' {..} = Prelude.rnf agentId

instance Data.ToHeaders GetAgentConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAgentConfiguration where
  toPath GetAgentConfiguration' {..} =
    Prelude.mconcat
      ["/agent/", Data.toBS agentId, "/configuration"]

instance Data.ToQuery GetAgentConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAgentConfigurationResponse' smart constructor.
data GetAgentConfigurationResponse = GetAgentConfigurationResponse'
  { -- | UUID of agent.
    agentId :: Prelude.Maybe Prelude.Text,
    -- | Tasking document for agent.
    taskingDocument :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAgentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentId', 'getAgentConfigurationResponse_agentId' - UUID of agent.
--
-- 'taskingDocument', 'getAgentConfigurationResponse_taskingDocument' - Tasking document for agent.
--
-- 'httpStatus', 'getAgentConfigurationResponse_httpStatus' - The response's http status code.
newGetAgentConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAgentConfigurationResponse
newGetAgentConfigurationResponse pHttpStatus_ =
  GetAgentConfigurationResponse'
    { agentId =
        Prelude.Nothing,
      taskingDocument = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | UUID of agent.
getAgentConfigurationResponse_agentId :: Lens.Lens' GetAgentConfigurationResponse (Prelude.Maybe Prelude.Text)
getAgentConfigurationResponse_agentId = Lens.lens (\GetAgentConfigurationResponse' {agentId} -> agentId) (\s@GetAgentConfigurationResponse' {} a -> s {agentId = a} :: GetAgentConfigurationResponse)

-- | Tasking document for agent.
getAgentConfigurationResponse_taskingDocument :: Lens.Lens' GetAgentConfigurationResponse (Prelude.Maybe Prelude.Text)
getAgentConfigurationResponse_taskingDocument = Lens.lens (\GetAgentConfigurationResponse' {taskingDocument} -> taskingDocument) (\s@GetAgentConfigurationResponse' {} a -> s {taskingDocument = a} :: GetAgentConfigurationResponse)

-- | The response's http status code.
getAgentConfigurationResponse_httpStatus :: Lens.Lens' GetAgentConfigurationResponse Prelude.Int
getAgentConfigurationResponse_httpStatus = Lens.lens (\GetAgentConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAgentConfigurationResponse' {} a -> s {httpStatus = a} :: GetAgentConfigurationResponse)

instance Prelude.NFData GetAgentConfigurationResponse where
  rnf GetAgentConfigurationResponse' {..} =
    Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf taskingDocument
      `Prelude.seq` Prelude.rnf httpStatus
