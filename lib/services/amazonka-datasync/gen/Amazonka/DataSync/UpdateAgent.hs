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
-- Module      : Amazonka.DataSync.UpdateAgent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of an agent.
module Amazonka.DataSync.UpdateAgent
  ( -- * Creating a Request
    UpdateAgent (..),
    newUpdateAgent,

    -- * Request Lenses
    updateAgent_name,
    updateAgent_agentArn,

    -- * Destructuring the Response
    UpdateAgentResponse (..),
    newUpdateAgentResponse,

    -- * Response Lenses
    updateAgentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | UpdateAgentRequest
--
-- /See:/ 'newUpdateAgent' smart constructor.
data UpdateAgent = UpdateAgent'
  { -- | The name that you want to use to configure the agent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the agent to update.
    agentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateAgent_name' - The name that you want to use to configure the agent.
--
-- 'agentArn', 'updateAgent_agentArn' - The Amazon Resource Name (ARN) of the agent to update.
newUpdateAgent ::
  -- | 'agentArn'
  Prelude.Text ->
  UpdateAgent
newUpdateAgent pAgentArn_ =
  UpdateAgent'
    { name = Prelude.Nothing,
      agentArn = pAgentArn_
    }

-- | The name that you want to use to configure the agent.
updateAgent_name :: Lens.Lens' UpdateAgent (Prelude.Maybe Prelude.Text)
updateAgent_name = Lens.lens (\UpdateAgent' {name} -> name) (\s@UpdateAgent' {} a -> s {name = a} :: UpdateAgent)

-- | The Amazon Resource Name (ARN) of the agent to update.
updateAgent_agentArn :: Lens.Lens' UpdateAgent Prelude.Text
updateAgent_agentArn = Lens.lens (\UpdateAgent' {agentArn} -> agentArn) (\s@UpdateAgent' {} a -> s {agentArn = a} :: UpdateAgent)

instance Core.AWSRequest UpdateAgent where
  type AWSResponse UpdateAgent = UpdateAgentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAgentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAgent where
  hashWithSalt _salt UpdateAgent' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` agentArn

instance Prelude.NFData UpdateAgent where
  rnf UpdateAgent' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf agentArn

instance Data.ToHeaders UpdateAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.UpdateAgent" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAgent where
  toJSON UpdateAgent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("AgentArn" Data..= agentArn)
          ]
      )

instance Data.ToPath UpdateAgent where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAgent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAgentResponse' smart constructor.
data UpdateAgentResponse = UpdateAgentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAgentResponse_httpStatus' - The response's http status code.
newUpdateAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAgentResponse
newUpdateAgentResponse pHttpStatus_ =
  UpdateAgentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateAgentResponse_httpStatus :: Lens.Lens' UpdateAgentResponse Prelude.Int
updateAgentResponse_httpStatus = Lens.lens (\UpdateAgentResponse' {httpStatus} -> httpStatus) (\s@UpdateAgentResponse' {} a -> s {httpStatus = a} :: UpdateAgentResponse)

instance Prelude.NFData UpdateAgentResponse where
  rnf UpdateAgentResponse' {..} = Prelude.rnf httpStatus
