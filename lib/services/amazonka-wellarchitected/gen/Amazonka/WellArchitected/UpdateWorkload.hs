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
-- Module      : Amazonka.WellArchitected.UpdateWorkload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing workload.
module Amazonka.WellArchitected.UpdateWorkload
  ( -- * Creating a Request
    UpdateWorkload (..),
    newUpdateWorkload,

    -- * Request Lenses
    updateWorkload_discoveryConfig,
    updateWorkload_accountIds,
    updateWorkload_environment,
    updateWorkload_isReviewOwnerUpdateAcknowledged,
    updateWorkload_industry,
    updateWorkload_applications,
    updateWorkload_awsRegions,
    updateWorkload_workloadName,
    updateWorkload_reviewOwner,
    updateWorkload_nonAwsRegions,
    updateWorkload_description,
    updateWorkload_notes,
    updateWorkload_industryType,
    updateWorkload_architecturalDesign,
    updateWorkload_pillarPriorities,
    updateWorkload_improvementStatus,
    updateWorkload_workloadId,

    -- * Destructuring the Response
    UpdateWorkloadResponse (..),
    newUpdateWorkloadResponse,

    -- * Response Lenses
    updateWorkloadResponse_workload,
    updateWorkloadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to update a workload.
--
-- /See:/ 'newUpdateWorkload' smart constructor.
data UpdateWorkload = UpdateWorkload'
  { -- | Well-Architected discovery configuration settings to associate to the
    -- workload.
    discoveryConfig :: Prelude.Maybe WorkloadDiscoveryConfig,
    accountIds :: Prelude.Maybe [Prelude.Text],
    environment :: Prelude.Maybe WorkloadEnvironment,
    -- | Flag indicating whether the workload owner has acknowledged that the
    -- /Review owner/ field is required.
    --
    -- If a __Review owner__ is not added to the workload within 60 days of
    -- acknowledgement, access to the workload is restricted until an owner is
    -- added.
    isReviewOwnerUpdateAcknowledged :: Prelude.Maybe Prelude.Bool,
    industry :: Prelude.Maybe Prelude.Text,
    -- | List of AppRegistry application ARNs to associate to the workload.
    applications :: Prelude.Maybe [Prelude.Text],
    awsRegions :: Prelude.Maybe [Prelude.Text],
    workloadName :: Prelude.Maybe Prelude.Text,
    reviewOwner :: Prelude.Maybe Prelude.Text,
    nonAwsRegions :: Prelude.Maybe [Prelude.Text],
    description :: Prelude.Maybe Prelude.Text,
    notes :: Prelude.Maybe Prelude.Text,
    industryType :: Prelude.Maybe Prelude.Text,
    architecturalDesign :: Prelude.Maybe Prelude.Text,
    pillarPriorities :: Prelude.Maybe [Prelude.Text],
    improvementStatus :: Prelude.Maybe WorkloadImprovementStatus,
    workloadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryConfig', 'updateWorkload_discoveryConfig' - Well-Architected discovery configuration settings to associate to the
-- workload.
--
-- 'accountIds', 'updateWorkload_accountIds' - Undocumented member.
--
-- 'environment', 'updateWorkload_environment' - Undocumented member.
--
-- 'isReviewOwnerUpdateAcknowledged', 'updateWorkload_isReviewOwnerUpdateAcknowledged' - Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
--
-- 'industry', 'updateWorkload_industry' - Undocumented member.
--
-- 'applications', 'updateWorkload_applications' - List of AppRegistry application ARNs to associate to the workload.
--
-- 'awsRegions', 'updateWorkload_awsRegions' - Undocumented member.
--
-- 'workloadName', 'updateWorkload_workloadName' - Undocumented member.
--
-- 'reviewOwner', 'updateWorkload_reviewOwner' - Undocumented member.
--
-- 'nonAwsRegions', 'updateWorkload_nonAwsRegions' - Undocumented member.
--
-- 'description', 'updateWorkload_description' - Undocumented member.
--
-- 'notes', 'updateWorkload_notes' - Undocumented member.
--
-- 'industryType', 'updateWorkload_industryType' - Undocumented member.
--
-- 'architecturalDesign', 'updateWorkload_architecturalDesign' - Undocumented member.
--
-- 'pillarPriorities', 'updateWorkload_pillarPriorities' - Undocumented member.
--
-- 'improvementStatus', 'updateWorkload_improvementStatus' - Undocumented member.
--
-- 'workloadId', 'updateWorkload_workloadId' - Undocumented member.
newUpdateWorkload ::
  -- | 'workloadId'
  Prelude.Text ->
  UpdateWorkload
newUpdateWorkload pWorkloadId_ =
  UpdateWorkload'
    { discoveryConfig = Prelude.Nothing,
      accountIds = Prelude.Nothing,
      environment = Prelude.Nothing,
      isReviewOwnerUpdateAcknowledged = Prelude.Nothing,
      industry = Prelude.Nothing,
      applications = Prelude.Nothing,
      awsRegions = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      reviewOwner = Prelude.Nothing,
      nonAwsRegions = Prelude.Nothing,
      description = Prelude.Nothing,
      notes = Prelude.Nothing,
      industryType = Prelude.Nothing,
      architecturalDesign = Prelude.Nothing,
      pillarPriorities = Prelude.Nothing,
      improvementStatus = Prelude.Nothing,
      workloadId = pWorkloadId_
    }

-- | Well-Architected discovery configuration settings to associate to the
-- workload.
updateWorkload_discoveryConfig :: Lens.Lens' UpdateWorkload (Prelude.Maybe WorkloadDiscoveryConfig)
updateWorkload_discoveryConfig = Lens.lens (\UpdateWorkload' {discoveryConfig} -> discoveryConfig) (\s@UpdateWorkload' {} a -> s {discoveryConfig = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_accountIds :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_accountIds = Lens.lens (\UpdateWorkload' {accountIds} -> accountIds) (\s@UpdateWorkload' {} a -> s {accountIds = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_environment :: Lens.Lens' UpdateWorkload (Prelude.Maybe WorkloadEnvironment)
updateWorkload_environment = Lens.lens (\UpdateWorkload' {environment} -> environment) (\s@UpdateWorkload' {} a -> s {environment = a} :: UpdateWorkload)

-- | Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
updateWorkload_isReviewOwnerUpdateAcknowledged :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Bool)
updateWorkload_isReviewOwnerUpdateAcknowledged = Lens.lens (\UpdateWorkload' {isReviewOwnerUpdateAcknowledged} -> isReviewOwnerUpdateAcknowledged) (\s@UpdateWorkload' {} a -> s {isReviewOwnerUpdateAcknowledged = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_industry :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_industry = Lens.lens (\UpdateWorkload' {industry} -> industry) (\s@UpdateWorkload' {} a -> s {industry = a} :: UpdateWorkload)

-- | List of AppRegistry application ARNs to associate to the workload.
updateWorkload_applications :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_applications = Lens.lens (\UpdateWorkload' {applications} -> applications) (\s@UpdateWorkload' {} a -> s {applications = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_awsRegions :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_awsRegions = Lens.lens (\UpdateWorkload' {awsRegions} -> awsRegions) (\s@UpdateWorkload' {} a -> s {awsRegions = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_workloadName :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_workloadName = Lens.lens (\UpdateWorkload' {workloadName} -> workloadName) (\s@UpdateWorkload' {} a -> s {workloadName = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_reviewOwner :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_reviewOwner = Lens.lens (\UpdateWorkload' {reviewOwner} -> reviewOwner) (\s@UpdateWorkload' {} a -> s {reviewOwner = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_nonAwsRegions :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_nonAwsRegions = Lens.lens (\UpdateWorkload' {nonAwsRegions} -> nonAwsRegions) (\s@UpdateWorkload' {} a -> s {nonAwsRegions = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_description :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_description = Lens.lens (\UpdateWorkload' {description} -> description) (\s@UpdateWorkload' {} a -> s {description = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_notes :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_notes = Lens.lens (\UpdateWorkload' {notes} -> notes) (\s@UpdateWorkload' {} a -> s {notes = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_industryType :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_industryType = Lens.lens (\UpdateWorkload' {industryType} -> industryType) (\s@UpdateWorkload' {} a -> s {industryType = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_architecturalDesign :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_architecturalDesign = Lens.lens (\UpdateWorkload' {architecturalDesign} -> architecturalDesign) (\s@UpdateWorkload' {} a -> s {architecturalDesign = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_pillarPriorities :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_pillarPriorities = Lens.lens (\UpdateWorkload' {pillarPriorities} -> pillarPriorities) (\s@UpdateWorkload' {} a -> s {pillarPriorities = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_improvementStatus :: Lens.Lens' UpdateWorkload (Prelude.Maybe WorkloadImprovementStatus)
updateWorkload_improvementStatus = Lens.lens (\UpdateWorkload' {improvementStatus} -> improvementStatus) (\s@UpdateWorkload' {} a -> s {improvementStatus = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_workloadId :: Lens.Lens' UpdateWorkload Prelude.Text
updateWorkload_workloadId = Lens.lens (\UpdateWorkload' {workloadId} -> workloadId) (\s@UpdateWorkload' {} a -> s {workloadId = a} :: UpdateWorkload)

instance Core.AWSRequest UpdateWorkload where
  type
    AWSResponse UpdateWorkload =
      UpdateWorkloadResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkloadResponse'
            Prelude.<$> (x Data..?> "Workload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkload where
  hashWithSalt _salt UpdateWorkload' {..} =
    _salt `Prelude.hashWithSalt` discoveryConfig
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` isReviewOwnerUpdateAcknowledged
      `Prelude.hashWithSalt` industry
      `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` awsRegions
      `Prelude.hashWithSalt` workloadName
      `Prelude.hashWithSalt` reviewOwner
      `Prelude.hashWithSalt` nonAwsRegions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` industryType
      `Prelude.hashWithSalt` architecturalDesign
      `Prelude.hashWithSalt` pillarPriorities
      `Prelude.hashWithSalt` improvementStatus
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData UpdateWorkload where
  rnf UpdateWorkload' {..} =
    Prelude.rnf discoveryConfig
      `Prelude.seq` Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf isReviewOwnerUpdateAcknowledged
      `Prelude.seq` Prelude.rnf industry
      `Prelude.seq` Prelude.rnf applications
      `Prelude.seq` Prelude.rnf awsRegions
      `Prelude.seq` Prelude.rnf workloadName
      `Prelude.seq` Prelude.rnf reviewOwner
      `Prelude.seq` Prelude.rnf nonAwsRegions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf industryType
      `Prelude.seq` Prelude.rnf architecturalDesign
      `Prelude.seq` Prelude.rnf pillarPriorities
      `Prelude.seq` Prelude.rnf improvementStatus
      `Prelude.seq` Prelude.rnf workloadId

instance Data.ToHeaders UpdateWorkload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkload where
  toJSON UpdateWorkload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DiscoveryConfig" Data..=)
              Prelude.<$> discoveryConfig,
            ("AccountIds" Data..=) Prelude.<$> accountIds,
            ("Environment" Data..=) Prelude.<$> environment,
            ("IsReviewOwnerUpdateAcknowledged" Data..=)
              Prelude.<$> isReviewOwnerUpdateAcknowledged,
            ("Industry" Data..=) Prelude.<$> industry,
            ("Applications" Data..=) Prelude.<$> applications,
            ("AwsRegions" Data..=) Prelude.<$> awsRegions,
            ("WorkloadName" Data..=) Prelude.<$> workloadName,
            ("ReviewOwner" Data..=) Prelude.<$> reviewOwner,
            ("NonAwsRegions" Data..=) Prelude.<$> nonAwsRegions,
            ("Description" Data..=) Prelude.<$> description,
            ("Notes" Data..=) Prelude.<$> notes,
            ("IndustryType" Data..=) Prelude.<$> industryType,
            ("ArchitecturalDesign" Data..=)
              Prelude.<$> architecturalDesign,
            ("PillarPriorities" Data..=)
              Prelude.<$> pillarPriorities,
            ("ImprovementStatus" Data..=)
              Prelude.<$> improvementStatus
          ]
      )

instance Data.ToPath UpdateWorkload where
  toPath UpdateWorkload' {..} =
    Prelude.mconcat
      ["/workloads/", Data.toBS workloadId]

instance Data.ToQuery UpdateWorkload where
  toQuery = Prelude.const Prelude.mempty

-- | Output of an update workload call.
--
-- /See:/ 'newUpdateWorkloadResponse' smart constructor.
data UpdateWorkloadResponse = UpdateWorkloadResponse'
  { workload :: Prelude.Maybe Workload,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkloadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workload', 'updateWorkloadResponse_workload' - Undocumented member.
--
-- 'httpStatus', 'updateWorkloadResponse_httpStatus' - The response's http status code.
newUpdateWorkloadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkloadResponse
newUpdateWorkloadResponse pHttpStatus_ =
  UpdateWorkloadResponse'
    { workload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateWorkloadResponse_workload :: Lens.Lens' UpdateWorkloadResponse (Prelude.Maybe Workload)
updateWorkloadResponse_workload = Lens.lens (\UpdateWorkloadResponse' {workload} -> workload) (\s@UpdateWorkloadResponse' {} a -> s {workload = a} :: UpdateWorkloadResponse)

-- | The response's http status code.
updateWorkloadResponse_httpStatus :: Lens.Lens' UpdateWorkloadResponse Prelude.Int
updateWorkloadResponse_httpStatus = Lens.lens (\UpdateWorkloadResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkloadResponse' {} a -> s {httpStatus = a} :: UpdateWorkloadResponse)

instance Prelude.NFData UpdateWorkloadResponse where
  rnf UpdateWorkloadResponse' {..} =
    Prelude.rnf workload
      `Prelude.seq` Prelude.rnf httpStatus
