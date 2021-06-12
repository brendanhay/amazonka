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
-- Module      : Network.AWS.Discovery.StartDataCollectionByAgentIds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents or connectors to start collecting data.
module Network.AWS.Discovery.StartDataCollectionByAgentIds
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

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDataCollectionByAgentIds' smart constructor.
data StartDataCollectionByAgentIds = StartDataCollectionByAgentIds'
  { -- | The IDs of the agents or connectors from which to start collecting data.
    -- If you send a request to an agent\/connector ID that you do not have
    -- permission to contact, according to your AWS account, the service does
    -- not throw an exception. Instead, it returns the error in the
    -- /Description/ field. If you send a request to multiple
    -- agents\/connectors and you do not have permission to contact some of
    -- those agents\/connectors, the system does not throw an exception.
    -- Instead, the system shows @Failed@ in the /Description/ field.
    agentIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- permission to contact, according to your AWS account, the service does
-- not throw an exception. Instead, it returns the error in the
-- /Description/ field. If you send a request to multiple
-- agents\/connectors and you do not have permission to contact some of
-- those agents\/connectors, the system does not throw an exception.
-- Instead, the system shows @Failed@ in the /Description/ field.
newStartDataCollectionByAgentIds ::
  StartDataCollectionByAgentIds
newStartDataCollectionByAgentIds =
  StartDataCollectionByAgentIds'
    { agentIds =
        Core.mempty
    }

-- | The IDs of the agents or connectors from which to start collecting data.
-- If you send a request to an agent\/connector ID that you do not have
-- permission to contact, according to your AWS account, the service does
-- not throw an exception. Instead, it returns the error in the
-- /Description/ field. If you send a request to multiple
-- agents\/connectors and you do not have permission to contact some of
-- those agents\/connectors, the system does not throw an exception.
-- Instead, the system shows @Failed@ in the /Description/ field.
startDataCollectionByAgentIds_agentIds :: Lens.Lens' StartDataCollectionByAgentIds [Core.Text]
startDataCollectionByAgentIds_agentIds = Lens.lens (\StartDataCollectionByAgentIds' {agentIds} -> agentIds) (\s@StartDataCollectionByAgentIds' {} a -> s {agentIds = a} :: StartDataCollectionByAgentIds) Core.. Lens._Coerce

instance
  Core.AWSRequest
    StartDataCollectionByAgentIds
  where
  type
    AWSResponse StartDataCollectionByAgentIds =
      StartDataCollectionByAgentIdsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataCollectionByAgentIdsResponse'
            Core.<$> ( x Core..?> "agentsConfigurationStatus"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartDataCollectionByAgentIds

instance Core.NFData StartDataCollectionByAgentIds

instance Core.ToHeaders StartDataCollectionByAgentIds where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.StartDataCollectionByAgentIds" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartDataCollectionByAgentIds where
  toJSON StartDataCollectionByAgentIds' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("agentIds" Core..= agentIds)]
      )

instance Core.ToPath StartDataCollectionByAgentIds where
  toPath = Core.const "/"

instance Core.ToQuery StartDataCollectionByAgentIds where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartDataCollectionByAgentIdsResponse' smart constructor.
data StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse'
  { -- | Information about agents or the connector that were instructed to start
    -- collecting data. Information includes the agent\/connector ID, a
    -- description of the operation performed, and whether the agent\/connector
    -- configuration was updated.
    agentsConfigurationStatus :: Core.Maybe [AgentConfigurationStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StartDataCollectionByAgentIdsResponse
newStartDataCollectionByAgentIdsResponse pHttpStatus_ =
  StartDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about agents or the connector that were instructed to start
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation performed, and whether the agent\/connector
-- configuration was updated.
startDataCollectionByAgentIdsResponse_agentsConfigurationStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse (Core.Maybe [AgentConfigurationStatus])
startDataCollectionByAgentIdsResponse_agentsConfigurationStatus = Lens.lens (\StartDataCollectionByAgentIdsResponse' {agentsConfigurationStatus} -> agentsConfigurationStatus) (\s@StartDataCollectionByAgentIdsResponse' {} a -> s {agentsConfigurationStatus = a} :: StartDataCollectionByAgentIdsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
startDataCollectionByAgentIdsResponse_httpStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse Core.Int
startDataCollectionByAgentIdsResponse_httpStatus = Lens.lens (\StartDataCollectionByAgentIdsResponse' {httpStatus} -> httpStatus) (\s@StartDataCollectionByAgentIdsResponse' {} a -> s {httpStatus = a} :: StartDataCollectionByAgentIdsResponse)

instance
  Core.NFData
    StartDataCollectionByAgentIdsResponse
