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
-- Module      : Amazonka.Discovery.StartDataCollectionByAgentIds
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents or connectors to start collecting data.
module Amazonka.Discovery.StartDataCollectionByAgentIds
  ( -- * Creating a Request
    StartDataCollectionByAgentIds (..),
    newStartDataCollectionByAgentIds,

    -- * Request Lenses
    startDataCollectionByAgentIds_agentIds,

    -- * Destructuring the Response
    StartDataCollectionByAgentIdsResponse (..),
    newStartDataCollectionByAgentIdsResponse,

    -- * Response Lenses
    startDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    startDataCollectionByAgentIdsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDataCollectionByAgentIds' smart constructor.
data StartDataCollectionByAgentIds = StartDataCollectionByAgentIds'
  { -- | The IDs of the agents or connectors from which to start collecting data.
    -- If you send a request to an agent\/connector ID that you do not have
    -- permission to contact, according to your Amazon Web Services account,
    -- the service does not throw an exception. Instead, it returns the error
    -- in the /Description/ field. If you send a request to multiple
    -- agents\/connectors and you do not have permission to contact some of
    -- those agents\/connectors, the system does not throw an exception.
    -- Instead, the system shows @Failed@ in the /Description/ field.
    agentIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataCollectionByAgentIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentIds', 'startDataCollectionByAgentIds_agentIds' - The IDs of the agents or connectors from which to start collecting data.
-- If you send a request to an agent\/connector ID that you do not have
-- permission to contact, according to your Amazon Web Services account,
-- the service does not throw an exception. Instead, it returns the error
-- in the /Description/ field. If you send a request to multiple
-- agents\/connectors and you do not have permission to contact some of
-- those agents\/connectors, the system does not throw an exception.
-- Instead, the system shows @Failed@ in the /Description/ field.
newStartDataCollectionByAgentIds ::
  StartDataCollectionByAgentIds
newStartDataCollectionByAgentIds =
  StartDataCollectionByAgentIds'
    { agentIds =
        Prelude.mempty
    }

-- | The IDs of the agents or connectors from which to start collecting data.
-- If you send a request to an agent\/connector ID that you do not have
-- permission to contact, according to your Amazon Web Services account,
-- the service does not throw an exception. Instead, it returns the error
-- in the /Description/ field. If you send a request to multiple
-- agents\/connectors and you do not have permission to contact some of
-- those agents\/connectors, the system does not throw an exception.
-- Instead, the system shows @Failed@ in the /Description/ field.
startDataCollectionByAgentIds_agentIds :: Lens.Lens' StartDataCollectionByAgentIds [Prelude.Text]
startDataCollectionByAgentIds_agentIds = Lens.lens (\StartDataCollectionByAgentIds' {agentIds} -> agentIds) (\s@StartDataCollectionByAgentIds' {} a -> s {agentIds = a} :: StartDataCollectionByAgentIds) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    StartDataCollectionByAgentIds
  where
  type
    AWSResponse StartDataCollectionByAgentIds =
      StartDataCollectionByAgentIdsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataCollectionByAgentIdsResponse'
            Prelude.<$> ( x Data..?> "agentsConfigurationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDataCollectionByAgentIds
  where
  hashWithSalt _salt StartDataCollectionByAgentIds' {..} =
    _salt `Prelude.hashWithSalt` agentIds

instance Prelude.NFData StartDataCollectionByAgentIds where
  rnf StartDataCollectionByAgentIds' {..} =
    Prelude.rnf agentIds

instance Data.ToHeaders StartDataCollectionByAgentIds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.StartDataCollectionByAgentIds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDataCollectionByAgentIds where
  toJSON StartDataCollectionByAgentIds' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("agentIds" Data..= agentIds)]
      )

instance Data.ToPath StartDataCollectionByAgentIds where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDataCollectionByAgentIds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDataCollectionByAgentIdsResponse' smart constructor.
data StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse'
  { -- | Information about agents or the connector that were instructed to start
    -- collecting data. Information includes the agent\/connector ID, a
    -- description of the operation performed, and whether the agent\/connector
    -- configuration was updated.
    agentsConfigurationStatus :: Prelude.Maybe [AgentConfigurationStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataCollectionByAgentIdsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentsConfigurationStatus', 'startDataCollectionByAgentIdsResponse_agentsConfigurationStatus' - Information about agents or the connector that were instructed to start
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation performed, and whether the agent\/connector
-- configuration was updated.
--
-- 'httpStatus', 'startDataCollectionByAgentIdsResponse_httpStatus' - The response's http status code.
newStartDataCollectionByAgentIdsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDataCollectionByAgentIdsResponse
newStartDataCollectionByAgentIdsResponse pHttpStatus_ =
  StartDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about agents or the connector that were instructed to start
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation performed, and whether the agent\/connector
-- configuration was updated.
startDataCollectionByAgentIdsResponse_agentsConfigurationStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse (Prelude.Maybe [AgentConfigurationStatus])
startDataCollectionByAgentIdsResponse_agentsConfigurationStatus = Lens.lens (\StartDataCollectionByAgentIdsResponse' {agentsConfigurationStatus} -> agentsConfigurationStatus) (\s@StartDataCollectionByAgentIdsResponse' {} a -> s {agentsConfigurationStatus = a} :: StartDataCollectionByAgentIdsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startDataCollectionByAgentIdsResponse_httpStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse Prelude.Int
startDataCollectionByAgentIdsResponse_httpStatus = Lens.lens (\StartDataCollectionByAgentIdsResponse' {httpStatus} -> httpStatus) (\s@StartDataCollectionByAgentIdsResponse' {} a -> s {httpStatus = a} :: StartDataCollectionByAgentIdsResponse)

instance
  Prelude.NFData
    StartDataCollectionByAgentIdsResponse
  where
  rnf StartDataCollectionByAgentIdsResponse' {..} =
    Prelude.rnf agentsConfigurationStatus
      `Prelude.seq` Prelude.rnf httpStatus
