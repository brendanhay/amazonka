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
-- Module      : Amazonka.SimSpaceWeaver.DescribeSimulation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current state of the given simulation.
module Amazonka.SimSpaceWeaver.DescribeSimulation
  ( -- * Creating a Request
    DescribeSimulation (..),
    newDescribeSimulation,

    -- * Request Lenses
    describeSimulation_simulation,

    -- * Destructuring the Response
    DescribeSimulationResponse (..),
    newDescribeSimulationResponse,

    -- * Response Lenses
    describeSimulationResponse_arn,
    describeSimulationResponse_creationTime,
    describeSimulationResponse_description,
    describeSimulationResponse_executionId,
    describeSimulationResponse_liveSimulationState,
    describeSimulationResponse_loggingConfiguration,
    describeSimulationResponse_maximumDuration,
    describeSimulationResponse_name,
    describeSimulationResponse_roleArn,
    describeSimulationResponse_schemaError,
    describeSimulationResponse_schemaS3Location,
    describeSimulationResponse_status,
    describeSimulationResponse_targetStatus,
    describeSimulationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newDescribeSimulation' smart constructor.
data DescribeSimulation = DescribeSimulation'
  { -- | The name of the simulation.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSimulation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simulation', 'describeSimulation_simulation' - The name of the simulation.
newDescribeSimulation ::
  -- | 'simulation'
  Prelude.Text ->
  DescribeSimulation
newDescribeSimulation pSimulation_ =
  DescribeSimulation' {simulation = pSimulation_}

-- | The name of the simulation.
describeSimulation_simulation :: Lens.Lens' DescribeSimulation Prelude.Text
describeSimulation_simulation = Lens.lens (\DescribeSimulation' {simulation} -> simulation) (\s@DescribeSimulation' {} a -> s {simulation = a} :: DescribeSimulation)

instance Core.AWSRequest DescribeSimulation where
  type
    AWSResponse DescribeSimulation =
      DescribeSimulationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSimulationResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ExecutionId")
            Prelude.<*> (x Data..?> "LiveSimulationState")
            Prelude.<*> (x Data..?> "LoggingConfiguration")
            Prelude.<*> (x Data..?> "MaximumDuration")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "SchemaError")
            Prelude.<*> (x Data..?> "SchemaS3Location")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TargetStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSimulation where
  hashWithSalt _salt DescribeSimulation' {..} =
    _salt `Prelude.hashWithSalt` simulation

instance Prelude.NFData DescribeSimulation where
  rnf DescribeSimulation' {..} = Prelude.rnf simulation

instance Data.ToHeaders DescribeSimulation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeSimulation where
  toPath = Prelude.const "/describesimulation"

instance Data.ToQuery DescribeSimulation where
  toQuery DescribeSimulation' {..} =
    Prelude.mconcat ["simulation" Data.=: simulation]

-- | /See:/ 'newDescribeSimulationResponse' smart constructor.
data DescribeSimulationResponse = DescribeSimulationResponse'
  { -- | The Amazon Resource Name (ARN) of the simulation. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the simulation was created, expressed as the number of
    -- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
    -- 1, 1970).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the simulation.
    description :: Prelude.Maybe Prelude.Text,
    -- | A universally unique identifier (UUID) for this simulation.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | A collection of additional state information, such as domain and clock
    -- configuration.
    liveSimulationState :: Prelude.Maybe LiveSimulationState,
    -- | Settings that control how SimSpace Weaver handles your simulation log
    -- data.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The maximum running time of the simulation, specified as a number of
    -- months (m or M), hours (h or H), or days (d or D). The simulation stops
    -- when it reaches this limit.
    maximumDuration :: Prelude.Maybe Prelude.Text,
    -- | The name of the simulation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that the simulation assumes to perform actions. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/. For more information
    -- about IAM roles, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
    -- in the /Identity and Access Management User Guide/.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | An error message that SimSpace Weaver returns only if there is a problem
    -- with the simulation schema.
    schemaError :: Prelude.Maybe Prelude.Text,
    -- | The location of the simulation schema in Amazon Simple Storage Service
    -- (Amazon S3). For more information about Amazon S3, see the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
    -- .
    schemaS3Location :: Prelude.Maybe S3Location,
    -- | The current lifecycle state of the simulation.
    status :: Prelude.Maybe SimulationStatus,
    -- | The desired lifecycle state of the simulation.
    targetStatus :: Prelude.Maybe SimulationTargetStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSimulationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeSimulationResponse_arn' - The Amazon Resource Name (ARN) of the simulation. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationTime', 'describeSimulationResponse_creationTime' - The time when the simulation was created, expressed as the number of
-- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
-- 1, 1970).
--
-- 'description', 'describeSimulationResponse_description' - The description of the simulation.
--
-- 'executionId', 'describeSimulationResponse_executionId' - A universally unique identifier (UUID) for this simulation.
--
-- 'liveSimulationState', 'describeSimulationResponse_liveSimulationState' - A collection of additional state information, such as domain and clock
-- configuration.
--
-- 'loggingConfiguration', 'describeSimulationResponse_loggingConfiguration' - Settings that control how SimSpace Weaver handles your simulation log
-- data.
--
-- 'maximumDuration', 'describeSimulationResponse_maximumDuration' - The maximum running time of the simulation, specified as a number of
-- months (m or M), hours (h or H), or days (d or D). The simulation stops
-- when it reaches this limit.
--
-- 'name', 'describeSimulationResponse_name' - The name of the simulation.
--
-- 'roleArn', 'describeSimulationResponse_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that the simulation assumes to perform actions. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For more information
-- about IAM roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
-- in the /Identity and Access Management User Guide/.
--
-- 'schemaError', 'describeSimulationResponse_schemaError' - An error message that SimSpace Weaver returns only if there is a problem
-- with the simulation schema.
--
-- 'schemaS3Location', 'describeSimulationResponse_schemaS3Location' - The location of the simulation schema in Amazon Simple Storage Service
-- (Amazon S3). For more information about Amazon S3, see the
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
-- .
--
-- 'status', 'describeSimulationResponse_status' - The current lifecycle state of the simulation.
--
-- 'targetStatus', 'describeSimulationResponse_targetStatus' - The desired lifecycle state of the simulation.
--
-- 'httpStatus', 'describeSimulationResponse_httpStatus' - The response's http status code.
newDescribeSimulationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSimulationResponse
newDescribeSimulationResponse pHttpStatus_ =
  DescribeSimulationResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      executionId = Prelude.Nothing,
      liveSimulationState = Prelude.Nothing,
      loggingConfiguration = Prelude.Nothing,
      maximumDuration = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      schemaError = Prelude.Nothing,
      schemaS3Location = Prelude.Nothing,
      status = Prelude.Nothing,
      targetStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the simulation. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
describeSimulationResponse_arn :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_arn = Lens.lens (\DescribeSimulationResponse' {arn} -> arn) (\s@DescribeSimulationResponse' {} a -> s {arn = a} :: DescribeSimulationResponse)

-- | The time when the simulation was created, expressed as the number of
-- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
-- 1, 1970).
describeSimulationResponse_creationTime :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.UTCTime)
describeSimulationResponse_creationTime = Lens.lens (\DescribeSimulationResponse' {creationTime} -> creationTime) (\s@DescribeSimulationResponse' {} a -> s {creationTime = a} :: DescribeSimulationResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the simulation.
describeSimulationResponse_description :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_description = Lens.lens (\DescribeSimulationResponse' {description} -> description) (\s@DescribeSimulationResponse' {} a -> s {description = a} :: DescribeSimulationResponse)

-- | A universally unique identifier (UUID) for this simulation.
describeSimulationResponse_executionId :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_executionId = Lens.lens (\DescribeSimulationResponse' {executionId} -> executionId) (\s@DescribeSimulationResponse' {} a -> s {executionId = a} :: DescribeSimulationResponse)

-- | A collection of additional state information, such as domain and clock
-- configuration.
describeSimulationResponse_liveSimulationState :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe LiveSimulationState)
describeSimulationResponse_liveSimulationState = Lens.lens (\DescribeSimulationResponse' {liveSimulationState} -> liveSimulationState) (\s@DescribeSimulationResponse' {} a -> s {liveSimulationState = a} :: DescribeSimulationResponse)

-- | Settings that control how SimSpace Weaver handles your simulation log
-- data.
describeSimulationResponse_loggingConfiguration :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe LoggingConfiguration)
describeSimulationResponse_loggingConfiguration = Lens.lens (\DescribeSimulationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@DescribeSimulationResponse' {} a -> s {loggingConfiguration = a} :: DescribeSimulationResponse)

-- | The maximum running time of the simulation, specified as a number of
-- months (m or M), hours (h or H), or days (d or D). The simulation stops
-- when it reaches this limit.
describeSimulationResponse_maximumDuration :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_maximumDuration = Lens.lens (\DescribeSimulationResponse' {maximumDuration} -> maximumDuration) (\s@DescribeSimulationResponse' {} a -> s {maximumDuration = a} :: DescribeSimulationResponse)

-- | The name of the simulation.
describeSimulationResponse_name :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_name = Lens.lens (\DescribeSimulationResponse' {name} -> name) (\s@DescribeSimulationResponse' {} a -> s {name = a} :: DescribeSimulationResponse)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that the simulation assumes to perform actions. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For more information
-- about IAM roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
-- in the /Identity and Access Management User Guide/.
describeSimulationResponse_roleArn :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_roleArn = Lens.lens (\DescribeSimulationResponse' {roleArn} -> roleArn) (\s@DescribeSimulationResponse' {} a -> s {roleArn = a} :: DescribeSimulationResponse)

-- | An error message that SimSpace Weaver returns only if there is a problem
-- with the simulation schema.
describeSimulationResponse_schemaError :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe Prelude.Text)
describeSimulationResponse_schemaError = Lens.lens (\DescribeSimulationResponse' {schemaError} -> schemaError) (\s@DescribeSimulationResponse' {} a -> s {schemaError = a} :: DescribeSimulationResponse)

-- | The location of the simulation schema in Amazon Simple Storage Service
-- (Amazon S3). For more information about Amazon S3, see the
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
-- .
describeSimulationResponse_schemaS3Location :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe S3Location)
describeSimulationResponse_schemaS3Location = Lens.lens (\DescribeSimulationResponse' {schemaS3Location} -> schemaS3Location) (\s@DescribeSimulationResponse' {} a -> s {schemaS3Location = a} :: DescribeSimulationResponse)

-- | The current lifecycle state of the simulation.
describeSimulationResponse_status :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe SimulationStatus)
describeSimulationResponse_status = Lens.lens (\DescribeSimulationResponse' {status} -> status) (\s@DescribeSimulationResponse' {} a -> s {status = a} :: DescribeSimulationResponse)

-- | The desired lifecycle state of the simulation.
describeSimulationResponse_targetStatus :: Lens.Lens' DescribeSimulationResponse (Prelude.Maybe SimulationTargetStatus)
describeSimulationResponse_targetStatus = Lens.lens (\DescribeSimulationResponse' {targetStatus} -> targetStatus) (\s@DescribeSimulationResponse' {} a -> s {targetStatus = a} :: DescribeSimulationResponse)

-- | The response's http status code.
describeSimulationResponse_httpStatus :: Lens.Lens' DescribeSimulationResponse Prelude.Int
describeSimulationResponse_httpStatus = Lens.lens (\DescribeSimulationResponse' {httpStatus} -> httpStatus) (\s@DescribeSimulationResponse' {} a -> s {httpStatus = a} :: DescribeSimulationResponse)

instance Prelude.NFData DescribeSimulationResponse where
  rnf DescribeSimulationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf liveSimulationState
      `Prelude.seq` Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf maximumDuration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf schemaError
      `Prelude.seq` Prelude.rnf schemaS3Location
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetStatus
      `Prelude.seq` Prelude.rnf httpStatus
