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
-- Module      : Amazonka.CodeGuruProfiler.UpdateProfilingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a profiling group.
module Amazonka.CodeGuruProfiler.UpdateProfilingGroup
  ( -- * Creating a Request
    UpdateProfilingGroup (..),
    newUpdateProfilingGroup,

    -- * Request Lenses
    updateProfilingGroup_agentOrchestrationConfig,
    updateProfilingGroup_profilingGroupName,

    -- * Destructuring the Response
    UpdateProfilingGroupResponse (..),
    newUpdateProfilingGroupResponse,

    -- * Response Lenses
    updateProfilingGroupResponse_httpStatus,
    updateProfilingGroupResponse_profilingGroup,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the updateProfilingGroupRequest.
--
-- /See:/ 'newUpdateProfilingGroup' smart constructor.
data UpdateProfilingGroup = UpdateProfilingGroup'
  { -- | Specifies whether profiling is enabled or disabled for a profiling
    -- group.
    agentOrchestrationConfig :: AgentOrchestrationConfig,
    -- | The name of the profiling group to update.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfilingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentOrchestrationConfig', 'updateProfilingGroup_agentOrchestrationConfig' - Specifies whether profiling is enabled or disabled for a profiling
-- group.
--
-- 'profilingGroupName', 'updateProfilingGroup_profilingGroupName' - The name of the profiling group to update.
newUpdateProfilingGroup ::
  -- | 'agentOrchestrationConfig'
  AgentOrchestrationConfig ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  UpdateProfilingGroup
newUpdateProfilingGroup
  pAgentOrchestrationConfig_
  pProfilingGroupName_ =
    UpdateProfilingGroup'
      { agentOrchestrationConfig =
          pAgentOrchestrationConfig_,
        profilingGroupName = pProfilingGroupName_
      }

-- | Specifies whether profiling is enabled or disabled for a profiling
-- group.
updateProfilingGroup_agentOrchestrationConfig :: Lens.Lens' UpdateProfilingGroup AgentOrchestrationConfig
updateProfilingGroup_agentOrchestrationConfig = Lens.lens (\UpdateProfilingGroup' {agentOrchestrationConfig} -> agentOrchestrationConfig) (\s@UpdateProfilingGroup' {} a -> s {agentOrchestrationConfig = a} :: UpdateProfilingGroup)

-- | The name of the profiling group to update.
updateProfilingGroup_profilingGroupName :: Lens.Lens' UpdateProfilingGroup Prelude.Text
updateProfilingGroup_profilingGroupName = Lens.lens (\UpdateProfilingGroup' {profilingGroupName} -> profilingGroupName) (\s@UpdateProfilingGroup' {} a -> s {profilingGroupName = a} :: UpdateProfilingGroup)

instance Core.AWSRequest UpdateProfilingGroup where
  type
    AWSResponse UpdateProfilingGroup =
      UpdateProfilingGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProfilingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateProfilingGroup where
  hashWithSalt _salt UpdateProfilingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` agentOrchestrationConfig
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData UpdateProfilingGroup where
  rnf UpdateProfilingGroup' {..} =
    Prelude.rnf agentOrchestrationConfig
      `Prelude.seq` Prelude.rnf profilingGroupName

instance Core.ToHeaders UpdateProfilingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProfilingGroup where
  toJSON UpdateProfilingGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "agentOrchestrationConfig"
                  Core..= agentOrchestrationConfig
              )
          ]
      )

instance Core.ToPath UpdateProfilingGroup where
  toPath UpdateProfilingGroup' {..} =
    Prelude.mconcat
      ["/profilingGroups/", Core.toBS profilingGroupName]

instance Core.ToQuery UpdateProfilingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the updateProfilingGroupResponse.
--
-- /See:/ 'newUpdateProfilingGroupResponse' smart constructor.
data UpdateProfilingGroupResponse = UpdateProfilingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
    -- that contains information about the returned updated profiling group.
    profilingGroup :: ProfilingGroupDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfilingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProfilingGroupResponse_httpStatus' - The response's http status code.
--
-- 'profilingGroup', 'updateProfilingGroupResponse_profilingGroup' - A
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- that contains information about the returned updated profiling group.
newUpdateProfilingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profilingGroup'
  ProfilingGroupDescription ->
  UpdateProfilingGroupResponse
newUpdateProfilingGroupResponse
  pHttpStatus_
  pProfilingGroup_ =
    UpdateProfilingGroupResponse'
      { httpStatus =
          pHttpStatus_,
        profilingGroup = pProfilingGroup_
      }

-- | The response's http status code.
updateProfilingGroupResponse_httpStatus :: Lens.Lens' UpdateProfilingGroupResponse Prelude.Int
updateProfilingGroupResponse_httpStatus = Lens.lens (\UpdateProfilingGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateProfilingGroupResponse' {} a -> s {httpStatus = a} :: UpdateProfilingGroupResponse)

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- that contains information about the returned updated profiling group.
updateProfilingGroupResponse_profilingGroup :: Lens.Lens' UpdateProfilingGroupResponse ProfilingGroupDescription
updateProfilingGroupResponse_profilingGroup = Lens.lens (\UpdateProfilingGroupResponse' {profilingGroup} -> profilingGroup) (\s@UpdateProfilingGroupResponse' {} a -> s {profilingGroup = a} :: UpdateProfilingGroupResponse)

instance Prelude.NFData UpdateProfilingGroupResponse where
  rnf UpdateProfilingGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profilingGroup
