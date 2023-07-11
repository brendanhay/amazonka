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
-- Module      : Amazonka.CodeGuruProfiler.ConfigureAgent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by profiler agents to report their current state and to receive
-- remote configuration updates. For example, @ConfigureAgent@ can be used
-- to tell an agent whether to profile or not and for how long to return
-- profiling data.
module Amazonka.CodeGuruProfiler.ConfigureAgent
  ( -- * Creating a Request
    ConfigureAgent (..),
    newConfigureAgent,

    -- * Request Lenses
    configureAgent_fleetInstanceId,
    configureAgent_metadata,
    configureAgent_profilingGroupName,

    -- * Destructuring the Response
    ConfigureAgentResponse (..),
    newConfigureAgentResponse,

    -- * Response Lenses
    configureAgentResponse_httpStatus,
    configureAgentResponse_configuration,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the configureAgentRequest.
--
-- /See:/ 'newConfigureAgent' smart constructor.
data ConfigureAgent = ConfigureAgent'
  { -- | A universally unique identifier (UUID) for a profiling instance. For
    -- example, if the profiling instance is an Amazon EC2 instance, it is the
    -- instance ID. If it is an AWS Fargate container, it is the container\'s
    -- task ID.
    fleetInstanceId :: Prelude.Maybe Prelude.Text,
    -- | Metadata captured about the compute platform the agent is running on. It
    -- includes information about sampling and reporting. The valid fields are:
    --
    -- -   @COMPUTE_PLATFORM@ - The compute platform on which the agent is
    --     running
    --
    -- -   @AGENT_ID@ - The ID for an agent instance.
    --
    -- -   @AWS_REQUEST_ID@ - The AWS request ID of a Lambda invocation.
    --
    -- -   @EXECUTION_ENVIRONMENT@ - The execution environment a Lambda
    --     function is running on.
    --
    -- -   @LAMBDA_FUNCTION_ARN@ - The Amazon Resource Name (ARN) that is used
    --     to invoke a Lambda function.
    --
    -- -   @LAMBDA_MEMORY_LIMIT_IN_MB@ - The memory allocated to a Lambda
    --     function.
    --
    -- -   @LAMBDA_REMAINING_TIME_IN_MILLISECONDS@ - The time in milliseconds
    --     before execution of a Lambda function times out.
    --
    -- -   @LAMBDA_TIME_GAP_BETWEEN_INVOKES_IN_MILLISECONDS@ - The time in
    --     milliseconds between two invocations of a Lambda function.
    --
    -- -   @LAMBDA_PREVIOUS_EXECUTION_TIME_IN_MILLISECONDS@ - The time in
    --     milliseconds for the previous Lambda invocation.
    metadata :: Prelude.Maybe (Prelude.HashMap MetadataField Prelude.Text),
    -- | The name of the profiling group for which the configured agent is
    -- collecting profiling data.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetInstanceId', 'configureAgent_fleetInstanceId' - A universally unique identifier (UUID) for a profiling instance. For
-- example, if the profiling instance is an Amazon EC2 instance, it is the
-- instance ID. If it is an AWS Fargate container, it is the container\'s
-- task ID.
--
-- 'metadata', 'configureAgent_metadata' - Metadata captured about the compute platform the agent is running on. It
-- includes information about sampling and reporting. The valid fields are:
--
-- -   @COMPUTE_PLATFORM@ - The compute platform on which the agent is
--     running
--
-- -   @AGENT_ID@ - The ID for an agent instance.
--
-- -   @AWS_REQUEST_ID@ - The AWS request ID of a Lambda invocation.
--
-- -   @EXECUTION_ENVIRONMENT@ - The execution environment a Lambda
--     function is running on.
--
-- -   @LAMBDA_FUNCTION_ARN@ - The Amazon Resource Name (ARN) that is used
--     to invoke a Lambda function.
--
-- -   @LAMBDA_MEMORY_LIMIT_IN_MB@ - The memory allocated to a Lambda
--     function.
--
-- -   @LAMBDA_REMAINING_TIME_IN_MILLISECONDS@ - The time in milliseconds
--     before execution of a Lambda function times out.
--
-- -   @LAMBDA_TIME_GAP_BETWEEN_INVOKES_IN_MILLISECONDS@ - The time in
--     milliseconds between two invocations of a Lambda function.
--
-- -   @LAMBDA_PREVIOUS_EXECUTION_TIME_IN_MILLISECONDS@ - The time in
--     milliseconds for the previous Lambda invocation.
--
-- 'profilingGroupName', 'configureAgent_profilingGroupName' - The name of the profiling group for which the configured agent is
-- collecting profiling data.
newConfigureAgent ::
  -- | 'profilingGroupName'
  Prelude.Text ->
  ConfigureAgent
newConfigureAgent pProfilingGroupName_ =
  ConfigureAgent'
    { fleetInstanceId = Prelude.Nothing,
      metadata = Prelude.Nothing,
      profilingGroupName = pProfilingGroupName_
    }

-- | A universally unique identifier (UUID) for a profiling instance. For
-- example, if the profiling instance is an Amazon EC2 instance, it is the
-- instance ID. If it is an AWS Fargate container, it is the container\'s
-- task ID.
configureAgent_fleetInstanceId :: Lens.Lens' ConfigureAgent (Prelude.Maybe Prelude.Text)
configureAgent_fleetInstanceId = Lens.lens (\ConfigureAgent' {fleetInstanceId} -> fleetInstanceId) (\s@ConfigureAgent' {} a -> s {fleetInstanceId = a} :: ConfigureAgent)

-- | Metadata captured about the compute platform the agent is running on. It
-- includes information about sampling and reporting. The valid fields are:
--
-- -   @COMPUTE_PLATFORM@ - The compute platform on which the agent is
--     running
--
-- -   @AGENT_ID@ - The ID for an agent instance.
--
-- -   @AWS_REQUEST_ID@ - The AWS request ID of a Lambda invocation.
--
-- -   @EXECUTION_ENVIRONMENT@ - The execution environment a Lambda
--     function is running on.
--
-- -   @LAMBDA_FUNCTION_ARN@ - The Amazon Resource Name (ARN) that is used
--     to invoke a Lambda function.
--
-- -   @LAMBDA_MEMORY_LIMIT_IN_MB@ - The memory allocated to a Lambda
--     function.
--
-- -   @LAMBDA_REMAINING_TIME_IN_MILLISECONDS@ - The time in milliseconds
--     before execution of a Lambda function times out.
--
-- -   @LAMBDA_TIME_GAP_BETWEEN_INVOKES_IN_MILLISECONDS@ - The time in
--     milliseconds between two invocations of a Lambda function.
--
-- -   @LAMBDA_PREVIOUS_EXECUTION_TIME_IN_MILLISECONDS@ - The time in
--     milliseconds for the previous Lambda invocation.
configureAgent_metadata :: Lens.Lens' ConfigureAgent (Prelude.Maybe (Prelude.HashMap MetadataField Prelude.Text))
configureAgent_metadata = Lens.lens (\ConfigureAgent' {metadata} -> metadata) (\s@ConfigureAgent' {} a -> s {metadata = a} :: ConfigureAgent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the profiling group for which the configured agent is
-- collecting profiling data.
configureAgent_profilingGroupName :: Lens.Lens' ConfigureAgent Prelude.Text
configureAgent_profilingGroupName = Lens.lens (\ConfigureAgent' {profilingGroupName} -> profilingGroupName) (\s@ConfigureAgent' {} a -> s {profilingGroupName = a} :: ConfigureAgent)

instance Core.AWSRequest ConfigureAgent where
  type
    AWSResponse ConfigureAgent =
      ConfigureAgentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfigureAgentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable ConfigureAgent where
  hashWithSalt _salt ConfigureAgent' {..} =
    _salt
      `Prelude.hashWithSalt` fleetInstanceId
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData ConfigureAgent where
  rnf ConfigureAgent' {..} =
    Prelude.rnf fleetInstanceId
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf profilingGroupName

instance Data.ToHeaders ConfigureAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfigureAgent where
  toJSON ConfigureAgent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fleetInstanceId" Data..=)
              Prelude.<$> fleetInstanceId,
            ("metadata" Data..=) Prelude.<$> metadata
          ]
      )

instance Data.ToPath ConfigureAgent where
  toPath ConfigureAgent' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/configureAgent"
      ]

instance Data.ToQuery ConfigureAgent where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the configureAgentResponse.
--
-- /See:/ 'newConfigureAgentResponse' smart constructor.
data ConfigureAgentResponse = ConfigureAgentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentConfiguration.html AgentConfiguration>
    -- object that specifies if an agent profiles or not and for how long to
    -- return profiling data.
    configuration :: AgentConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'configureAgentResponse_httpStatus' - The response's http status code.
--
-- 'configuration', 'configureAgentResponse_configuration' - An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentConfiguration.html AgentConfiguration>
-- object that specifies if an agent profiles or not and for how long to
-- return profiling data.
newConfigureAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuration'
  AgentConfiguration ->
  ConfigureAgentResponse
newConfigureAgentResponse
  pHttpStatus_
  pConfiguration_ =
    ConfigureAgentResponse'
      { httpStatus = pHttpStatus_,
        configuration = pConfiguration_
      }

-- | The response's http status code.
configureAgentResponse_httpStatus :: Lens.Lens' ConfigureAgentResponse Prelude.Int
configureAgentResponse_httpStatus = Lens.lens (\ConfigureAgentResponse' {httpStatus} -> httpStatus) (\s@ConfigureAgentResponse' {} a -> s {httpStatus = a} :: ConfigureAgentResponse)

-- | An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentConfiguration.html AgentConfiguration>
-- object that specifies if an agent profiles or not and for how long to
-- return profiling data.
configureAgentResponse_configuration :: Lens.Lens' ConfigureAgentResponse AgentConfiguration
configureAgentResponse_configuration = Lens.lens (\ConfigureAgentResponse' {configuration} -> configuration) (\s@ConfigureAgentResponse' {} a -> s {configuration = a} :: ConfigureAgentResponse)

instance Prelude.NFData ConfigureAgentResponse where
  rnf ConfigureAgentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuration
