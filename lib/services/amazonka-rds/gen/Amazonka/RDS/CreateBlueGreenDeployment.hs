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
-- Module      : Amazonka.RDS.CreateBlueGreenDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a blue\/green deployment.
--
-- A blue\/green deployment creates a staging environment that copies the
-- production environment. In a blue\/green deployment, the blue
-- environment is the current production environment. The green environment
-- is the staging environment. The staging environment stays in sync with
-- the current production environment using logical replication.
--
-- You can make changes to the databases in the green environment without
-- affecting production workloads. For example, you can upgrade the major
-- or minor DB engine version, change database parameters, or make schema
-- changes in the staging environment. You can thoroughly test changes in
-- the green environment. When ready, you can switch over the environments
-- to promote the green environment to be the new production environment.
-- The switchover typically takes under a minute.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.CreateBlueGreenDeployment
  ( -- * Creating a Request
    CreateBlueGreenDeployment (..),
    newCreateBlueGreenDeployment,

    -- * Request Lenses
    createBlueGreenDeployment_tags,
    createBlueGreenDeployment_targetDBClusterParameterGroupName,
    createBlueGreenDeployment_targetDBParameterGroupName,
    createBlueGreenDeployment_targetEngineVersion,
    createBlueGreenDeployment_blueGreenDeploymentName,
    createBlueGreenDeployment_source,

    -- * Destructuring the Response
    CreateBlueGreenDeploymentResponse (..),
    newCreateBlueGreenDeploymentResponse,

    -- * Response Lenses
    createBlueGreenDeploymentResponse_blueGreenDeployment,
    createBlueGreenDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBlueGreenDeployment' smart constructor.
data CreateBlueGreenDeployment = CreateBlueGreenDeployment'
  { -- | Tags to assign to the blue\/green deployment.
    tags :: Prelude.Maybe [Tag],
    -- | The DB cluster parameter group associated with the Aurora DB cluster in
    -- the green environment.
    --
    -- To test parameter changes, specify a DB cluster parameter group that is
    -- different from the one associated with the source DB cluster.
    targetDBClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The DB parameter group associated with the DB instance in the green
    -- environment.
    --
    -- To test parameter changes, specify a DB parameter group that is
    -- different from the one associated with the source DB instance.
    targetDBParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The engine version of the database in the green environment.
    --
    -- Specify the engine version to upgrade to in the green environment.
    targetEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the blue\/green deployment.
    --
    -- Constraints:
    --
    -- -   Can\'t be the same as an existing blue\/green deployment name in the
    --     same account and Amazon Web Services Region.
    blueGreenDeploymentName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source production database.
    --
    -- Specify the database that you want to clone. The blue\/green deployment
    -- creates this database in the green environment. You can make updates to
    -- the database in the green environment, such as an engine version
    -- upgrade. When you are ready, you can switch the database in the green
    -- environment to be the production database.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBlueGreenDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createBlueGreenDeployment_tags' - Tags to assign to the blue\/green deployment.
--
-- 'targetDBClusterParameterGroupName', 'createBlueGreenDeployment_targetDBClusterParameterGroupName' - The DB cluster parameter group associated with the Aurora DB cluster in
-- the green environment.
--
-- To test parameter changes, specify a DB cluster parameter group that is
-- different from the one associated with the source DB cluster.
--
-- 'targetDBParameterGroupName', 'createBlueGreenDeployment_targetDBParameterGroupName' - The DB parameter group associated with the DB instance in the green
-- environment.
--
-- To test parameter changes, specify a DB parameter group that is
-- different from the one associated with the source DB instance.
--
-- 'targetEngineVersion', 'createBlueGreenDeployment_targetEngineVersion' - The engine version of the database in the green environment.
--
-- Specify the engine version to upgrade to in the green environment.
--
-- 'blueGreenDeploymentName', 'createBlueGreenDeployment_blueGreenDeploymentName' - The name of the blue\/green deployment.
--
-- Constraints:
--
-- -   Can\'t be the same as an existing blue\/green deployment name in the
--     same account and Amazon Web Services Region.
--
-- 'source', 'createBlueGreenDeployment_source' - The Amazon Resource Name (ARN) of the source production database.
--
-- Specify the database that you want to clone. The blue\/green deployment
-- creates this database in the green environment. You can make updates to
-- the database in the green environment, such as an engine version
-- upgrade. When you are ready, you can switch the database in the green
-- environment to be the production database.
newCreateBlueGreenDeployment ::
  -- | 'blueGreenDeploymentName'
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  CreateBlueGreenDeployment
newCreateBlueGreenDeployment
  pBlueGreenDeploymentName_
  pSource_ =
    CreateBlueGreenDeployment'
      { tags = Prelude.Nothing,
        targetDBClusterParameterGroupName =
          Prelude.Nothing,
        targetDBParameterGroupName = Prelude.Nothing,
        targetEngineVersion = Prelude.Nothing,
        blueGreenDeploymentName =
          pBlueGreenDeploymentName_,
        source = pSource_
      }

-- | Tags to assign to the blue\/green deployment.
createBlueGreenDeployment_tags :: Lens.Lens' CreateBlueGreenDeployment (Prelude.Maybe [Tag])
createBlueGreenDeployment_tags = Lens.lens (\CreateBlueGreenDeployment' {tags} -> tags) (\s@CreateBlueGreenDeployment' {} a -> s {tags = a} :: CreateBlueGreenDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster parameter group associated with the Aurora DB cluster in
-- the green environment.
--
-- To test parameter changes, specify a DB cluster parameter group that is
-- different from the one associated with the source DB cluster.
createBlueGreenDeployment_targetDBClusterParameterGroupName :: Lens.Lens' CreateBlueGreenDeployment (Prelude.Maybe Prelude.Text)
createBlueGreenDeployment_targetDBClusterParameterGroupName = Lens.lens (\CreateBlueGreenDeployment' {targetDBClusterParameterGroupName} -> targetDBClusterParameterGroupName) (\s@CreateBlueGreenDeployment' {} a -> s {targetDBClusterParameterGroupName = a} :: CreateBlueGreenDeployment)

-- | The DB parameter group associated with the DB instance in the green
-- environment.
--
-- To test parameter changes, specify a DB parameter group that is
-- different from the one associated with the source DB instance.
createBlueGreenDeployment_targetDBParameterGroupName :: Lens.Lens' CreateBlueGreenDeployment (Prelude.Maybe Prelude.Text)
createBlueGreenDeployment_targetDBParameterGroupName = Lens.lens (\CreateBlueGreenDeployment' {targetDBParameterGroupName} -> targetDBParameterGroupName) (\s@CreateBlueGreenDeployment' {} a -> s {targetDBParameterGroupName = a} :: CreateBlueGreenDeployment)

-- | The engine version of the database in the green environment.
--
-- Specify the engine version to upgrade to in the green environment.
createBlueGreenDeployment_targetEngineVersion :: Lens.Lens' CreateBlueGreenDeployment (Prelude.Maybe Prelude.Text)
createBlueGreenDeployment_targetEngineVersion = Lens.lens (\CreateBlueGreenDeployment' {targetEngineVersion} -> targetEngineVersion) (\s@CreateBlueGreenDeployment' {} a -> s {targetEngineVersion = a} :: CreateBlueGreenDeployment)

-- | The name of the blue\/green deployment.
--
-- Constraints:
--
-- -   Can\'t be the same as an existing blue\/green deployment name in the
--     same account and Amazon Web Services Region.
createBlueGreenDeployment_blueGreenDeploymentName :: Lens.Lens' CreateBlueGreenDeployment Prelude.Text
createBlueGreenDeployment_blueGreenDeploymentName = Lens.lens (\CreateBlueGreenDeployment' {blueGreenDeploymentName} -> blueGreenDeploymentName) (\s@CreateBlueGreenDeployment' {} a -> s {blueGreenDeploymentName = a} :: CreateBlueGreenDeployment)

-- | The Amazon Resource Name (ARN) of the source production database.
--
-- Specify the database that you want to clone. The blue\/green deployment
-- creates this database in the green environment. You can make updates to
-- the database in the green environment, such as an engine version
-- upgrade. When you are ready, you can switch the database in the green
-- environment to be the production database.
createBlueGreenDeployment_source :: Lens.Lens' CreateBlueGreenDeployment Prelude.Text
createBlueGreenDeployment_source = Lens.lens (\CreateBlueGreenDeployment' {source} -> source) (\s@CreateBlueGreenDeployment' {} a -> s {source = a} :: CreateBlueGreenDeployment)

instance Core.AWSRequest CreateBlueGreenDeployment where
  type
    AWSResponse CreateBlueGreenDeployment =
      CreateBlueGreenDeploymentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateBlueGreenDeploymentResult"
      ( \s h x ->
          CreateBlueGreenDeploymentResponse'
            Prelude.<$> (x Data..@? "BlueGreenDeployment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBlueGreenDeployment where
  hashWithSalt _salt CreateBlueGreenDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetDBClusterParameterGroupName
      `Prelude.hashWithSalt` targetDBParameterGroupName
      `Prelude.hashWithSalt` targetEngineVersion
      `Prelude.hashWithSalt` blueGreenDeploymentName
      `Prelude.hashWithSalt` source

instance Prelude.NFData CreateBlueGreenDeployment where
  rnf CreateBlueGreenDeployment' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf targetDBClusterParameterGroupName `Prelude.seq`
        Prelude.rnf targetDBParameterGroupName `Prelude.seq`
          Prelude.rnf targetEngineVersion `Prelude.seq`
            Prelude.rnf blueGreenDeploymentName `Prelude.seq`
              Prelude.rnf source

instance Data.ToHeaders CreateBlueGreenDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateBlueGreenDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBlueGreenDeployment where
  toQuery CreateBlueGreenDeployment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateBlueGreenDeployment" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "TargetDBClusterParameterGroupName"
          Data.=: targetDBClusterParameterGroupName,
        "TargetDBParameterGroupName"
          Data.=: targetDBParameterGroupName,
        "TargetEngineVersion" Data.=: targetEngineVersion,
        "BlueGreenDeploymentName"
          Data.=: blueGreenDeploymentName,
        "Source" Data.=: source
      ]

-- | /See:/ 'newCreateBlueGreenDeploymentResponse' smart constructor.
data CreateBlueGreenDeploymentResponse = CreateBlueGreenDeploymentResponse'
  { blueGreenDeployment :: Prelude.Maybe BlueGreenDeployment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBlueGreenDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueGreenDeployment', 'createBlueGreenDeploymentResponse_blueGreenDeployment' - Undocumented member.
--
-- 'httpStatus', 'createBlueGreenDeploymentResponse_httpStatus' - The response's http status code.
newCreateBlueGreenDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBlueGreenDeploymentResponse
newCreateBlueGreenDeploymentResponse pHttpStatus_ =
  CreateBlueGreenDeploymentResponse'
    { blueGreenDeployment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createBlueGreenDeploymentResponse_blueGreenDeployment :: Lens.Lens' CreateBlueGreenDeploymentResponse (Prelude.Maybe BlueGreenDeployment)
createBlueGreenDeploymentResponse_blueGreenDeployment = Lens.lens (\CreateBlueGreenDeploymentResponse' {blueGreenDeployment} -> blueGreenDeployment) (\s@CreateBlueGreenDeploymentResponse' {} a -> s {blueGreenDeployment = a} :: CreateBlueGreenDeploymentResponse)

-- | The response's http status code.
createBlueGreenDeploymentResponse_httpStatus :: Lens.Lens' CreateBlueGreenDeploymentResponse Prelude.Int
createBlueGreenDeploymentResponse_httpStatus = Lens.lens (\CreateBlueGreenDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateBlueGreenDeploymentResponse' {} a -> s {httpStatus = a} :: CreateBlueGreenDeploymentResponse)

instance
  Prelude.NFData
    CreateBlueGreenDeploymentResponse
  where
  rnf CreateBlueGreenDeploymentResponse' {..} =
    Prelude.rnf blueGreenDeployment `Prelude.seq`
      Prelude.rnf httpStatus
