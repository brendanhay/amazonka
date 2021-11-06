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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    updateWorkload_isReviewOwnerUpdateAcknowledged,
    updateWorkload_architecturalDesign,
    updateWorkload_accountIds,
    updateWorkload_industry,
    updateWorkload_environment,
    updateWorkload_awsRegions,
    updateWorkload_improvementStatus,
    updateWorkload_industryType,
    updateWorkload_workloadName,
    updateWorkload_notes,
    updateWorkload_reviewOwner,
    updateWorkload_description,
    updateWorkload_pillarPriorities,
    updateWorkload_nonAwsRegions,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to update a workload.
--
-- /See:/ 'newUpdateWorkload' smart constructor.
data UpdateWorkload = UpdateWorkload'
  { -- | Flag indicating whether the workload owner has acknowledged that the
    -- /Review owner/ field is required.
    --
    -- If a __Review owner__ is not added to the workload within 60 days of
    -- acknowledgement, access to the workload is restricted until an owner is
    -- added.
    isReviewOwnerUpdateAcknowledged :: Prelude.Maybe Prelude.Bool,
    architecturalDesign :: Prelude.Maybe Prelude.Text,
    accountIds :: Prelude.Maybe [Prelude.Text],
    industry :: Prelude.Maybe Prelude.Text,
    environment :: Prelude.Maybe WorkloadEnvironment,
    awsRegions :: Prelude.Maybe [Prelude.Text],
    improvementStatus :: Prelude.Maybe WorkloadImprovementStatus,
    industryType :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text,
    notes :: Prelude.Maybe Prelude.Text,
    reviewOwner :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    pillarPriorities :: Prelude.Maybe [Prelude.Text],
    nonAwsRegions :: Prelude.Maybe [Prelude.Text],
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
-- 'isReviewOwnerUpdateAcknowledged', 'updateWorkload_isReviewOwnerUpdateAcknowledged' - Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
--
-- 'architecturalDesign', 'updateWorkload_architecturalDesign' - Undocumented member.
--
-- 'accountIds', 'updateWorkload_accountIds' - Undocumented member.
--
-- 'industry', 'updateWorkload_industry' - Undocumented member.
--
-- 'environment', 'updateWorkload_environment' - Undocumented member.
--
-- 'awsRegions', 'updateWorkload_awsRegions' - Undocumented member.
--
-- 'improvementStatus', 'updateWorkload_improvementStatus' - Undocumented member.
--
-- 'industryType', 'updateWorkload_industryType' - Undocumented member.
--
-- 'workloadName', 'updateWorkload_workloadName' - Undocumented member.
--
-- 'notes', 'updateWorkload_notes' - Undocumented member.
--
-- 'reviewOwner', 'updateWorkload_reviewOwner' - Undocumented member.
--
-- 'description', 'updateWorkload_description' - Undocumented member.
--
-- 'pillarPriorities', 'updateWorkload_pillarPriorities' - Undocumented member.
--
-- 'nonAwsRegions', 'updateWorkload_nonAwsRegions' - Undocumented member.
--
-- 'workloadId', 'updateWorkload_workloadId' - Undocumented member.
newUpdateWorkload ::
  -- | 'workloadId'
  Prelude.Text ->
  UpdateWorkload
newUpdateWorkload pWorkloadId_ =
  UpdateWorkload'
    { isReviewOwnerUpdateAcknowledged =
        Prelude.Nothing,
      architecturalDesign = Prelude.Nothing,
      accountIds = Prelude.Nothing,
      industry = Prelude.Nothing,
      environment = Prelude.Nothing,
      awsRegions = Prelude.Nothing,
      improvementStatus = Prelude.Nothing,
      industryType = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      notes = Prelude.Nothing,
      reviewOwner = Prelude.Nothing,
      description = Prelude.Nothing,
      pillarPriorities = Prelude.Nothing,
      nonAwsRegions = Prelude.Nothing,
      workloadId = pWorkloadId_
    }

-- | Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
updateWorkload_isReviewOwnerUpdateAcknowledged :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Bool)
updateWorkload_isReviewOwnerUpdateAcknowledged = Lens.lens (\UpdateWorkload' {isReviewOwnerUpdateAcknowledged} -> isReviewOwnerUpdateAcknowledged) (\s@UpdateWorkload' {} a -> s {isReviewOwnerUpdateAcknowledged = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_architecturalDesign :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_architecturalDesign = Lens.lens (\UpdateWorkload' {architecturalDesign} -> architecturalDesign) (\s@UpdateWorkload' {} a -> s {architecturalDesign = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_accountIds :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_accountIds = Lens.lens (\UpdateWorkload' {accountIds} -> accountIds) (\s@UpdateWorkload' {} a -> s {accountIds = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_industry :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_industry = Lens.lens (\UpdateWorkload' {industry} -> industry) (\s@UpdateWorkload' {} a -> s {industry = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_environment :: Lens.Lens' UpdateWorkload (Prelude.Maybe WorkloadEnvironment)
updateWorkload_environment = Lens.lens (\UpdateWorkload' {environment} -> environment) (\s@UpdateWorkload' {} a -> s {environment = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_awsRegions :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_awsRegions = Lens.lens (\UpdateWorkload' {awsRegions} -> awsRegions) (\s@UpdateWorkload' {} a -> s {awsRegions = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_improvementStatus :: Lens.Lens' UpdateWorkload (Prelude.Maybe WorkloadImprovementStatus)
updateWorkload_improvementStatus = Lens.lens (\UpdateWorkload' {improvementStatus} -> improvementStatus) (\s@UpdateWorkload' {} a -> s {improvementStatus = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_industryType :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_industryType = Lens.lens (\UpdateWorkload' {industryType} -> industryType) (\s@UpdateWorkload' {} a -> s {industryType = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_workloadName :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_workloadName = Lens.lens (\UpdateWorkload' {workloadName} -> workloadName) (\s@UpdateWorkload' {} a -> s {workloadName = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_notes :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_notes = Lens.lens (\UpdateWorkload' {notes} -> notes) (\s@UpdateWorkload' {} a -> s {notes = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_reviewOwner :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_reviewOwner = Lens.lens (\UpdateWorkload' {reviewOwner} -> reviewOwner) (\s@UpdateWorkload' {} a -> s {reviewOwner = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_description :: Lens.Lens' UpdateWorkload (Prelude.Maybe Prelude.Text)
updateWorkload_description = Lens.lens (\UpdateWorkload' {description} -> description) (\s@UpdateWorkload' {} a -> s {description = a} :: UpdateWorkload)

-- | Undocumented member.
updateWorkload_pillarPriorities :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_pillarPriorities = Lens.lens (\UpdateWorkload' {pillarPriorities} -> pillarPriorities) (\s@UpdateWorkload' {} a -> s {pillarPriorities = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_nonAwsRegions :: Lens.Lens' UpdateWorkload (Prelude.Maybe [Prelude.Text])
updateWorkload_nonAwsRegions = Lens.lens (\UpdateWorkload' {nonAwsRegions} -> nonAwsRegions) (\s@UpdateWorkload' {} a -> s {nonAwsRegions = a} :: UpdateWorkload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateWorkload_workloadId :: Lens.Lens' UpdateWorkload Prelude.Text
updateWorkload_workloadId = Lens.lens (\UpdateWorkload' {workloadId} -> workloadId) (\s@UpdateWorkload' {} a -> s {workloadId = a} :: UpdateWorkload)

instance Core.AWSRequest UpdateWorkload where
  type
    AWSResponse UpdateWorkload =
      UpdateWorkloadResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkloadResponse'
            Prelude.<$> (x Core..?> "Workload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkload

instance Prelude.NFData UpdateWorkload

instance Core.ToHeaders UpdateWorkload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateWorkload where
  toJSON UpdateWorkload' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IsReviewOwnerUpdateAcknowledged" Core..=)
              Prelude.<$> isReviewOwnerUpdateAcknowledged,
            ("ArchitecturalDesign" Core..=)
              Prelude.<$> architecturalDesign,
            ("AccountIds" Core..=) Prelude.<$> accountIds,
            ("Industry" Core..=) Prelude.<$> industry,
            ("Environment" Core..=) Prelude.<$> environment,
            ("AwsRegions" Core..=) Prelude.<$> awsRegions,
            ("ImprovementStatus" Core..=)
              Prelude.<$> improvementStatus,
            ("IndustryType" Core..=) Prelude.<$> industryType,
            ("WorkloadName" Core..=) Prelude.<$> workloadName,
            ("Notes" Core..=) Prelude.<$> notes,
            ("ReviewOwner" Core..=) Prelude.<$> reviewOwner,
            ("Description" Core..=) Prelude.<$> description,
            ("PillarPriorities" Core..=)
              Prelude.<$> pillarPriorities,
            ("NonAwsRegions" Core..=) Prelude.<$> nonAwsRegions
          ]
      )

instance Core.ToPath UpdateWorkload where
  toPath UpdateWorkload' {..} =
    Prelude.mconcat
      ["/workloads/", Core.toBS workloadId]

instance Core.ToQuery UpdateWorkload where
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

instance Prelude.NFData UpdateWorkloadResponse
