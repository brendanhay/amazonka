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
-- Module      : Amazonka.SimSpaceWeaver.StartSimulation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a simulation with the given name and schema.
module Amazonka.SimSpaceWeaver.StartSimulation
  ( -- * Creating a Request
    StartSimulation (..),
    newStartSimulation,

    -- * Request Lenses
    startSimulation_clientToken,
    startSimulation_description,
    startSimulation_maximumDuration,
    startSimulation_tags,
    startSimulation_name,
    startSimulation_roleArn,
    startSimulation_schemaS3Location,

    -- * Destructuring the Response
    StartSimulationResponse (..),
    newStartSimulationResponse,

    -- * Response Lenses
    startSimulationResponse_arn,
    startSimulationResponse_creationTime,
    startSimulationResponse_executionId,
    startSimulationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newStartSimulation' smart constructor.
data StartSimulation = StartSimulation'
  { -- | A value that you provide to ensure that repeated calls to this API
    -- operation using the same parameters complete only once. A @ClientToken@
    -- is also known as an /idempotency token/. A @ClientToken@ expires after
    -- 24 hours.
    clientToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The description of the simulation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum running time of the simulation, specified as a number of
    -- months (m or M), hours (h or H), or days (d or D). The simulation stops
    -- when it reaches this limit.
    maximumDuration :: Prelude.Maybe Prelude.Text,
    -- | A list of tags for the simulation. For more information about tags, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the simulation.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that the simulation assumes to perform actions. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/. For more information
    -- about IAM roles, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
    -- in the /Identity and Access Management User Guide/.
    roleArn :: Prelude.Text,
    -- | The location of the simulation schema in Amazon Simple Storage Service
    -- (Amazon S3). For more information about Amazon S3, see the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
    -- .
    schemaS3Location :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSimulation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startSimulation_clientToken' - A value that you provide to ensure that repeated calls to this API
-- operation using the same parameters complete only once. A @ClientToken@
-- is also known as an /idempotency token/. A @ClientToken@ expires after
-- 24 hours.
--
-- 'description', 'startSimulation_description' - The description of the simulation.
--
-- 'maximumDuration', 'startSimulation_maximumDuration' - The maximum running time of the simulation, specified as a number of
-- months (m or M), hours (h or H), or days (d or D). The simulation stops
-- when it reaches this limit.
--
-- 'tags', 'startSimulation_tags' - A list of tags for the simulation. For more information about tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
--
-- 'name', 'startSimulation_name' - The name of the simulation.
--
-- 'roleArn', 'startSimulation_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that the simulation assumes to perform actions. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For more information
-- about IAM roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
-- in the /Identity and Access Management User Guide/.
--
-- 'schemaS3Location', 'startSimulation_schemaS3Location' - The location of the simulation schema in Amazon Simple Storage Service
-- (Amazon S3). For more information about Amazon S3, see the
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
-- .
newStartSimulation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'schemaS3Location'
  S3Location ->
  StartSimulation
newStartSimulation
  pName_
  pRoleArn_
  pSchemaS3Location_ =
    StartSimulation'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        maximumDuration = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        roleArn = pRoleArn_,
        schemaS3Location = pSchemaS3Location_
      }

-- | A value that you provide to ensure that repeated calls to this API
-- operation using the same parameters complete only once. A @ClientToken@
-- is also known as an /idempotency token/. A @ClientToken@ expires after
-- 24 hours.
startSimulation_clientToken :: Lens.Lens' StartSimulation (Prelude.Maybe Prelude.Text)
startSimulation_clientToken = Lens.lens (\StartSimulation' {clientToken} -> clientToken) (\s@StartSimulation' {} a -> s {clientToken = a} :: StartSimulation) Prelude.. Lens.mapping Data._Sensitive

-- | The description of the simulation.
startSimulation_description :: Lens.Lens' StartSimulation (Prelude.Maybe Prelude.Text)
startSimulation_description = Lens.lens (\StartSimulation' {description} -> description) (\s@StartSimulation' {} a -> s {description = a} :: StartSimulation)

-- | The maximum running time of the simulation, specified as a number of
-- months (m or M), hours (h or H), or days (d or D). The simulation stops
-- when it reaches this limit.
startSimulation_maximumDuration :: Lens.Lens' StartSimulation (Prelude.Maybe Prelude.Text)
startSimulation_maximumDuration = Lens.lens (\StartSimulation' {maximumDuration} -> maximumDuration) (\s@StartSimulation' {} a -> s {maximumDuration = a} :: StartSimulation)

-- | A list of tags for the simulation. For more information about tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
startSimulation_tags :: Lens.Lens' StartSimulation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startSimulation_tags = Lens.lens (\StartSimulation' {tags} -> tags) (\s@StartSimulation' {} a -> s {tags = a} :: StartSimulation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the simulation.
startSimulation_name :: Lens.Lens' StartSimulation Prelude.Text
startSimulation_name = Lens.lens (\StartSimulation' {name} -> name) (\s@StartSimulation' {} a -> s {name = a} :: StartSimulation)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that the simulation assumes to perform actions. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For more information
-- about IAM roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
-- in the /Identity and Access Management User Guide/.
startSimulation_roleArn :: Lens.Lens' StartSimulation Prelude.Text
startSimulation_roleArn = Lens.lens (\StartSimulation' {roleArn} -> roleArn) (\s@StartSimulation' {} a -> s {roleArn = a} :: StartSimulation)

-- | The location of the simulation schema in Amazon Simple Storage Service
-- (Amazon S3). For more information about Amazon S3, see the
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
-- .
startSimulation_schemaS3Location :: Lens.Lens' StartSimulation S3Location
startSimulation_schemaS3Location = Lens.lens (\StartSimulation' {schemaS3Location} -> schemaS3Location) (\s@StartSimulation' {} a -> s {schemaS3Location = a} :: StartSimulation)

instance Core.AWSRequest StartSimulation where
  type
    AWSResponse StartSimulation =
      StartSimulationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSimulationResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSimulation where
  hashWithSalt _salt StartSimulation' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maximumDuration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` schemaS3Location

instance Prelude.NFData StartSimulation where
  rnf StartSimulation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maximumDuration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf schemaS3Location

instance Data.ToHeaders StartSimulation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSimulation where
  toJSON StartSimulation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("MaximumDuration" Data..=)
              Prelude.<$> maximumDuration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just
              ("SchemaS3Location" Data..= schemaS3Location)
          ]
      )

instance Data.ToPath StartSimulation where
  toPath = Prelude.const "/startsimulation"

instance Data.ToQuery StartSimulation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSimulationResponse' smart constructor.
data StartSimulationResponse = StartSimulationResponse'
  { -- | The Amazon Resource Name (ARN) of the simulation. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the simulation was created, expressed as the number of
    -- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
    -- 1, 1970).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A universally unique identifier (UUID) for this simulation.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSimulationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startSimulationResponse_arn' - The Amazon Resource Name (ARN) of the simulation. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationTime', 'startSimulationResponse_creationTime' - The time when the simulation was created, expressed as the number of
-- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
-- 1, 1970).
--
-- 'executionId', 'startSimulationResponse_executionId' - A universally unique identifier (UUID) for this simulation.
--
-- 'httpStatus', 'startSimulationResponse_httpStatus' - The response's http status code.
newStartSimulationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSimulationResponse
newStartSimulationResponse pHttpStatus_ =
  StartSimulationResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      executionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the simulation. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
startSimulationResponse_arn :: Lens.Lens' StartSimulationResponse (Prelude.Maybe Prelude.Text)
startSimulationResponse_arn = Lens.lens (\StartSimulationResponse' {arn} -> arn) (\s@StartSimulationResponse' {} a -> s {arn = a} :: StartSimulationResponse)

-- | The time when the simulation was created, expressed as the number of
-- seconds and milliseconds in UTC since the Unix epoch (0:0:0.000, January
-- 1, 1970).
startSimulationResponse_creationTime :: Lens.Lens' StartSimulationResponse (Prelude.Maybe Prelude.UTCTime)
startSimulationResponse_creationTime = Lens.lens (\StartSimulationResponse' {creationTime} -> creationTime) (\s@StartSimulationResponse' {} a -> s {creationTime = a} :: StartSimulationResponse) Prelude.. Lens.mapping Data._Time

-- | A universally unique identifier (UUID) for this simulation.
startSimulationResponse_executionId :: Lens.Lens' StartSimulationResponse (Prelude.Maybe Prelude.Text)
startSimulationResponse_executionId = Lens.lens (\StartSimulationResponse' {executionId} -> executionId) (\s@StartSimulationResponse' {} a -> s {executionId = a} :: StartSimulationResponse)

-- | The response's http status code.
startSimulationResponse_httpStatus :: Lens.Lens' StartSimulationResponse Prelude.Int
startSimulationResponse_httpStatus = Lens.lens (\StartSimulationResponse' {httpStatus} -> httpStatus) (\s@StartSimulationResponse' {} a -> s {httpStatus = a} :: StartSimulationResponse)

instance Prelude.NFData StartSimulationResponse where
  rnf StartSimulationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf httpStatus
