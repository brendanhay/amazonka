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
-- Module      : Amazonka.WellArchitected.CreateWorkload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new workload.
--
-- The owner of a workload can share the workload with other Amazon Web
-- Services accounts, IAM users, an organization, and organizational units
-- (OUs) in the same Amazon Web Services Region. Only the owner of a
-- workload can delete it.
--
-- For more information, see
-- <https://docs.aws.amazon.com/wellarchitected/latest/userguide/define-workload.html Defining a Workload>
-- in the /Well-Architected Tool User Guide/.
module Amazonka.WellArchitected.CreateWorkload
  ( -- * Creating a Request
    CreateWorkload (..),
    newCreateWorkload,

    -- * Request Lenses
    createWorkload_accountIds,
    createWorkload_applications,
    createWorkload_architecturalDesign,
    createWorkload_awsRegions,
    createWorkload_discoveryConfig,
    createWorkload_industry,
    createWorkload_industryType,
    createWorkload_nonAwsRegions,
    createWorkload_notes,
    createWorkload_pillarPriorities,
    createWorkload_reviewOwner,
    createWorkload_tags,
    createWorkload_workloadName,
    createWorkload_description,
    createWorkload_environment,
    createWorkload_lenses,
    createWorkload_clientRequestToken,

    -- * Destructuring the Response
    CreateWorkloadResponse (..),
    newCreateWorkloadResponse,

    -- * Response Lenses
    createWorkloadResponse_workloadArn,
    createWorkloadResponse_workloadId,
    createWorkloadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for workload creation.
--
-- /See:/ 'newCreateWorkload' smart constructor.
data CreateWorkload = CreateWorkload'
  { accountIds :: Prelude.Maybe [Prelude.Text],
    -- | List of AppRegistry application ARNs associated to the workload.
    applications :: Prelude.Maybe [Prelude.Text],
    architecturalDesign :: Prelude.Maybe Prelude.Text,
    awsRegions :: Prelude.Maybe [Prelude.Text],
    -- | Well-Architected discovery configuration settings associated to the
    -- workload.
    discoveryConfig :: Prelude.Maybe WorkloadDiscoveryConfig,
    industry :: Prelude.Maybe Prelude.Text,
    industryType :: Prelude.Maybe Prelude.Text,
    nonAwsRegions :: Prelude.Maybe [Prelude.Text],
    notes :: Prelude.Maybe Prelude.Text,
    pillarPriorities :: Prelude.Maybe [Prelude.Text],
    reviewOwner :: Prelude.Maybe Prelude.Text,
    -- | The tags to be associated with the workload.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    workloadName :: Prelude.Text,
    description :: Prelude.Text,
    environment :: WorkloadEnvironment,
    lenses :: [Prelude.Text],
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'createWorkload_accountIds' - Undocumented member.
--
-- 'applications', 'createWorkload_applications' - List of AppRegistry application ARNs associated to the workload.
--
-- 'architecturalDesign', 'createWorkload_architecturalDesign' - Undocumented member.
--
-- 'awsRegions', 'createWorkload_awsRegions' - Undocumented member.
--
-- 'discoveryConfig', 'createWorkload_discoveryConfig' - Well-Architected discovery configuration settings associated to the
-- workload.
--
-- 'industry', 'createWorkload_industry' - Undocumented member.
--
-- 'industryType', 'createWorkload_industryType' - Undocumented member.
--
-- 'nonAwsRegions', 'createWorkload_nonAwsRegions' - Undocumented member.
--
-- 'notes', 'createWorkload_notes' - Undocumented member.
--
-- 'pillarPriorities', 'createWorkload_pillarPriorities' - Undocumented member.
--
-- 'reviewOwner', 'createWorkload_reviewOwner' - Undocumented member.
--
-- 'tags', 'createWorkload_tags' - The tags to be associated with the workload.
--
-- 'workloadName', 'createWorkload_workloadName' - Undocumented member.
--
-- 'description', 'createWorkload_description' - Undocumented member.
--
-- 'environment', 'createWorkload_environment' - Undocumented member.
--
-- 'lenses', 'createWorkload_lenses' - Undocumented member.
--
-- 'clientRequestToken', 'createWorkload_clientRequestToken' - Undocumented member.
newCreateWorkload ::
  -- | 'workloadName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'environment'
  WorkloadEnvironment ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateWorkload
newCreateWorkload
  pWorkloadName_
  pDescription_
  pEnvironment_
  pClientRequestToken_ =
    CreateWorkload'
      { accountIds = Prelude.Nothing,
        applications = Prelude.Nothing,
        architecturalDesign = Prelude.Nothing,
        awsRegions = Prelude.Nothing,
        discoveryConfig = Prelude.Nothing,
        industry = Prelude.Nothing,
        industryType = Prelude.Nothing,
        nonAwsRegions = Prelude.Nothing,
        notes = Prelude.Nothing,
        pillarPriorities = Prelude.Nothing,
        reviewOwner = Prelude.Nothing,
        tags = Prelude.Nothing,
        workloadName = pWorkloadName_,
        description = pDescription_,
        environment = pEnvironment_,
        lenses = Prelude.mempty,
        clientRequestToken = pClientRequestToken_
      }

-- | Undocumented member.
createWorkload_accountIds :: Lens.Lens' CreateWorkload (Prelude.Maybe [Prelude.Text])
createWorkload_accountIds = Lens.lens (\CreateWorkload' {accountIds} -> accountIds) (\s@CreateWorkload' {} a -> s {accountIds = a} :: CreateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | List of AppRegistry application ARNs associated to the workload.
createWorkload_applications :: Lens.Lens' CreateWorkload (Prelude.Maybe [Prelude.Text])
createWorkload_applications = Lens.lens (\CreateWorkload' {applications} -> applications) (\s@CreateWorkload' {} a -> s {applications = a} :: CreateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createWorkload_architecturalDesign :: Lens.Lens' CreateWorkload (Prelude.Maybe Prelude.Text)
createWorkload_architecturalDesign = Lens.lens (\CreateWorkload' {architecturalDesign} -> architecturalDesign) (\s@CreateWorkload' {} a -> s {architecturalDesign = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_awsRegions :: Lens.Lens' CreateWorkload (Prelude.Maybe [Prelude.Text])
createWorkload_awsRegions = Lens.lens (\CreateWorkload' {awsRegions} -> awsRegions) (\s@CreateWorkload' {} a -> s {awsRegions = a} :: CreateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Well-Architected discovery configuration settings associated to the
-- workload.
createWorkload_discoveryConfig :: Lens.Lens' CreateWorkload (Prelude.Maybe WorkloadDiscoveryConfig)
createWorkload_discoveryConfig = Lens.lens (\CreateWorkload' {discoveryConfig} -> discoveryConfig) (\s@CreateWorkload' {} a -> s {discoveryConfig = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_industry :: Lens.Lens' CreateWorkload (Prelude.Maybe Prelude.Text)
createWorkload_industry = Lens.lens (\CreateWorkload' {industry} -> industry) (\s@CreateWorkload' {} a -> s {industry = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_industryType :: Lens.Lens' CreateWorkload (Prelude.Maybe Prelude.Text)
createWorkload_industryType = Lens.lens (\CreateWorkload' {industryType} -> industryType) (\s@CreateWorkload' {} a -> s {industryType = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_nonAwsRegions :: Lens.Lens' CreateWorkload (Prelude.Maybe [Prelude.Text])
createWorkload_nonAwsRegions = Lens.lens (\CreateWorkload' {nonAwsRegions} -> nonAwsRegions) (\s@CreateWorkload' {} a -> s {nonAwsRegions = a} :: CreateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createWorkload_notes :: Lens.Lens' CreateWorkload (Prelude.Maybe Prelude.Text)
createWorkload_notes = Lens.lens (\CreateWorkload' {notes} -> notes) (\s@CreateWorkload' {} a -> s {notes = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_pillarPriorities :: Lens.Lens' CreateWorkload (Prelude.Maybe [Prelude.Text])
createWorkload_pillarPriorities = Lens.lens (\CreateWorkload' {pillarPriorities} -> pillarPriorities) (\s@CreateWorkload' {} a -> s {pillarPriorities = a} :: CreateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createWorkload_reviewOwner :: Lens.Lens' CreateWorkload (Prelude.Maybe Prelude.Text)
createWorkload_reviewOwner = Lens.lens (\CreateWorkload' {reviewOwner} -> reviewOwner) (\s@CreateWorkload' {} a -> s {reviewOwner = a} :: CreateWorkload)

-- | The tags to be associated with the workload.
createWorkload_tags :: Lens.Lens' CreateWorkload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkload_tags = Lens.lens (\CreateWorkload' {tags} -> tags) (\s@CreateWorkload' {} a -> s {tags = a} :: CreateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createWorkload_workloadName :: Lens.Lens' CreateWorkload Prelude.Text
createWorkload_workloadName = Lens.lens (\CreateWorkload' {workloadName} -> workloadName) (\s@CreateWorkload' {} a -> s {workloadName = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_description :: Lens.Lens' CreateWorkload Prelude.Text
createWorkload_description = Lens.lens (\CreateWorkload' {description} -> description) (\s@CreateWorkload' {} a -> s {description = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_environment :: Lens.Lens' CreateWorkload WorkloadEnvironment
createWorkload_environment = Lens.lens (\CreateWorkload' {environment} -> environment) (\s@CreateWorkload' {} a -> s {environment = a} :: CreateWorkload)

-- | Undocumented member.
createWorkload_lenses :: Lens.Lens' CreateWorkload [Prelude.Text]
createWorkload_lenses = Lens.lens (\CreateWorkload' {lenses} -> lenses) (\s@CreateWorkload' {} a -> s {lenses = a} :: CreateWorkload) Prelude.. Lens.coerced

-- | Undocumented member.
createWorkload_clientRequestToken :: Lens.Lens' CreateWorkload Prelude.Text
createWorkload_clientRequestToken = Lens.lens (\CreateWorkload' {clientRequestToken} -> clientRequestToken) (\s@CreateWorkload' {} a -> s {clientRequestToken = a} :: CreateWorkload)

instance Core.AWSRequest CreateWorkload where
  type
    AWSResponse CreateWorkload =
      CreateWorkloadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkloadResponse'
            Prelude.<$> (x Data..?> "WorkloadArn")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkload where
  hashWithSalt _salt CreateWorkload' {..} =
    _salt
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` architecturalDesign
      `Prelude.hashWithSalt` awsRegions
      `Prelude.hashWithSalt` discoveryConfig
      `Prelude.hashWithSalt` industry
      `Prelude.hashWithSalt` industryType
      `Prelude.hashWithSalt` nonAwsRegions
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` pillarPriorities
      `Prelude.hashWithSalt` reviewOwner
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workloadName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` lenses
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateWorkload where
  rnf CreateWorkload' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf applications
      `Prelude.seq` Prelude.rnf architecturalDesign
      `Prelude.seq` Prelude.rnf awsRegions
      `Prelude.seq` Prelude.rnf discoveryConfig
      `Prelude.seq` Prelude.rnf industry
      `Prelude.seq` Prelude.rnf industryType
      `Prelude.seq` Prelude.rnf nonAwsRegions
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf pillarPriorities
      `Prelude.seq` Prelude.rnf reviewOwner
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workloadName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf lenses
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateWorkload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkload where
  toJSON CreateWorkload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountIds" Data..=) Prelude.<$> accountIds,
            ("Applications" Data..=) Prelude.<$> applications,
            ("ArchitecturalDesign" Data..=)
              Prelude.<$> architecturalDesign,
            ("AwsRegions" Data..=) Prelude.<$> awsRegions,
            ("DiscoveryConfig" Data..=)
              Prelude.<$> discoveryConfig,
            ("Industry" Data..=) Prelude.<$> industry,
            ("IndustryType" Data..=) Prelude.<$> industryType,
            ("NonAwsRegions" Data..=) Prelude.<$> nonAwsRegions,
            ("Notes" Data..=) Prelude.<$> notes,
            ("PillarPriorities" Data..=)
              Prelude.<$> pillarPriorities,
            ("ReviewOwner" Data..=) Prelude.<$> reviewOwner,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("WorkloadName" Data..= workloadName),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just ("Environment" Data..= environment),
            Prelude.Just ("Lenses" Data..= lenses),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateWorkload where
  toPath = Prelude.const "/workloads"

instance Data.ToQuery CreateWorkload where
  toQuery = Prelude.const Prelude.mempty

-- | Output of a create workload call.
--
-- /See:/ 'newCreateWorkloadResponse' smart constructor.
data CreateWorkloadResponse = CreateWorkloadResponse'
  { workloadArn :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkloadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadArn', 'createWorkloadResponse_workloadArn' - Undocumented member.
--
-- 'workloadId', 'createWorkloadResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'createWorkloadResponse_httpStatus' - The response's http status code.
newCreateWorkloadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkloadResponse
newCreateWorkloadResponse pHttpStatus_ =
  CreateWorkloadResponse'
    { workloadArn =
        Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createWorkloadResponse_workloadArn :: Lens.Lens' CreateWorkloadResponse (Prelude.Maybe Prelude.Text)
createWorkloadResponse_workloadArn = Lens.lens (\CreateWorkloadResponse' {workloadArn} -> workloadArn) (\s@CreateWorkloadResponse' {} a -> s {workloadArn = a} :: CreateWorkloadResponse)

-- | Undocumented member.
createWorkloadResponse_workloadId :: Lens.Lens' CreateWorkloadResponse (Prelude.Maybe Prelude.Text)
createWorkloadResponse_workloadId = Lens.lens (\CreateWorkloadResponse' {workloadId} -> workloadId) (\s@CreateWorkloadResponse' {} a -> s {workloadId = a} :: CreateWorkloadResponse)

-- | The response's http status code.
createWorkloadResponse_httpStatus :: Lens.Lens' CreateWorkloadResponse Prelude.Int
createWorkloadResponse_httpStatus = Lens.lens (\CreateWorkloadResponse' {httpStatus} -> httpStatus) (\s@CreateWorkloadResponse' {} a -> s {httpStatus = a} :: CreateWorkloadResponse)

instance Prelude.NFData CreateWorkloadResponse where
  rnf CreateWorkloadResponse' {..} =
    Prelude.rnf workloadArn
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf httpStatus
