{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Types.Workload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Workload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.Risk
import Amazonka.WellArchitected.Types.WorkloadEnvironment
import Amazonka.WellArchitected.Types.WorkloadImprovementStatus

-- | A workload return object.
--
-- /See:/ 'newWorkload' smart constructor.
data Workload = Workload'
  { -- | Flag indicating whether the workload owner has acknowledged that the
    -- /Review owner/ field is required.
    --
    -- If a __Review owner__ is not added to the workload within 60 days of
    -- acknowledgement, access to the workload is restricted until an owner is
    -- added.
    isReviewOwnerUpdateAcknowledged :: Prelude.Maybe Prelude.Bool,
    architecturalDesign :: Prelude.Maybe Prelude.Text,
    accountIds :: Prelude.Maybe [Prelude.Text],
    lenses :: Prelude.Maybe [Prelude.Text],
    reviewRestrictionDate :: Prelude.Maybe Core.POSIX,
    industry :: Prelude.Maybe Prelude.Text,
    environment :: Prelude.Maybe WorkloadEnvironment,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    awsRegions :: Prelude.Maybe [Prelude.Text],
    owner :: Prelude.Maybe Prelude.Text,
    improvementStatus :: Prelude.Maybe WorkloadImprovementStatus,
    workloadArn :: Prelude.Maybe Prelude.Text,
    industryType :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text,
    updatedAt :: Prelude.Maybe Core.POSIX,
    notes :: Prelude.Maybe Prelude.Text,
    reviewOwner :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    pillarPriorities :: Prelude.Maybe [Prelude.Text],
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text,
    nonAwsRegions :: Prelude.Maybe [Prelude.Text],
    -- | The tags associated with the workload.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Workload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isReviewOwnerUpdateAcknowledged', 'workload_isReviewOwnerUpdateAcknowledged' - Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
--
-- 'architecturalDesign', 'workload_architecturalDesign' - Undocumented member.
--
-- 'accountIds', 'workload_accountIds' - Undocumented member.
--
-- 'lenses', 'workload_lenses' - Undocumented member.
--
-- 'reviewRestrictionDate', 'workload_reviewRestrictionDate' - Undocumented member.
--
-- 'industry', 'workload_industry' - Undocumented member.
--
-- 'environment', 'workload_environment' - Undocumented member.
--
-- 'riskCounts', 'workload_riskCounts' - Undocumented member.
--
-- 'awsRegions', 'workload_awsRegions' - Undocumented member.
--
-- 'owner', 'workload_owner' - Undocumented member.
--
-- 'improvementStatus', 'workload_improvementStatus' - Undocumented member.
--
-- 'workloadArn', 'workload_workloadArn' - Undocumented member.
--
-- 'industryType', 'workload_industryType' - Undocumented member.
--
-- 'workloadId', 'workload_workloadId' - Undocumented member.
--
-- 'workloadName', 'workload_workloadName' - Undocumented member.
--
-- 'updatedAt', 'workload_updatedAt' - Undocumented member.
--
-- 'notes', 'workload_notes' - Undocumented member.
--
-- 'reviewOwner', 'workload_reviewOwner' - Undocumented member.
--
-- 'description', 'workload_description' - Undocumented member.
--
-- 'pillarPriorities', 'workload_pillarPriorities' - Undocumented member.
--
-- 'shareInvitationId', 'workload_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'nonAwsRegions', 'workload_nonAwsRegions' - Undocumented member.
--
-- 'tags', 'workload_tags' - The tags associated with the workload.
newWorkload ::
  Workload
newWorkload =
  Workload'
    { isReviewOwnerUpdateAcknowledged =
        Prelude.Nothing,
      architecturalDesign = Prelude.Nothing,
      accountIds = Prelude.Nothing,
      lenses = Prelude.Nothing,
      reviewRestrictionDate = Prelude.Nothing,
      industry = Prelude.Nothing,
      environment = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      awsRegions = Prelude.Nothing,
      owner = Prelude.Nothing,
      improvementStatus = Prelude.Nothing,
      workloadArn = Prelude.Nothing,
      industryType = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      notes = Prelude.Nothing,
      reviewOwner = Prelude.Nothing,
      description = Prelude.Nothing,
      pillarPriorities = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing,
      nonAwsRegions = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
workload_isReviewOwnerUpdateAcknowledged :: Lens.Lens' Workload (Prelude.Maybe Prelude.Bool)
workload_isReviewOwnerUpdateAcknowledged = Lens.lens (\Workload' {isReviewOwnerUpdateAcknowledged} -> isReviewOwnerUpdateAcknowledged) (\s@Workload' {} a -> s {isReviewOwnerUpdateAcknowledged = a} :: Workload)

-- | Undocumented member.
workload_architecturalDesign :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_architecturalDesign = Lens.lens (\Workload' {architecturalDesign} -> architecturalDesign) (\s@Workload' {} a -> s {architecturalDesign = a} :: Workload)

-- | Undocumented member.
workload_accountIds :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_accountIds = Lens.lens (\Workload' {accountIds} -> accountIds) (\s@Workload' {} a -> s {accountIds = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_lenses :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_lenses = Lens.lens (\Workload' {lenses} -> lenses) (\s@Workload' {} a -> s {lenses = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_reviewRestrictionDate :: Lens.Lens' Workload (Prelude.Maybe Prelude.UTCTime)
workload_reviewRestrictionDate = Lens.lens (\Workload' {reviewRestrictionDate} -> reviewRestrictionDate) (\s@Workload' {} a -> s {reviewRestrictionDate = a} :: Workload) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
workload_industry :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_industry = Lens.lens (\Workload' {industry} -> industry) (\s@Workload' {} a -> s {industry = a} :: Workload)

-- | Undocumented member.
workload_environment :: Lens.Lens' Workload (Prelude.Maybe WorkloadEnvironment)
workload_environment = Lens.lens (\Workload' {environment} -> environment) (\s@Workload' {} a -> s {environment = a} :: Workload)

-- | Undocumented member.
workload_riskCounts :: Lens.Lens' Workload (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
workload_riskCounts = Lens.lens (\Workload' {riskCounts} -> riskCounts) (\s@Workload' {} a -> s {riskCounts = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_awsRegions :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_awsRegions = Lens.lens (\Workload' {awsRegions} -> awsRegions) (\s@Workload' {} a -> s {awsRegions = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_owner :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_owner = Lens.lens (\Workload' {owner} -> owner) (\s@Workload' {} a -> s {owner = a} :: Workload)

-- | Undocumented member.
workload_improvementStatus :: Lens.Lens' Workload (Prelude.Maybe WorkloadImprovementStatus)
workload_improvementStatus = Lens.lens (\Workload' {improvementStatus} -> improvementStatus) (\s@Workload' {} a -> s {improvementStatus = a} :: Workload)

-- | Undocumented member.
workload_workloadArn :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_workloadArn = Lens.lens (\Workload' {workloadArn} -> workloadArn) (\s@Workload' {} a -> s {workloadArn = a} :: Workload)

-- | Undocumented member.
workload_industryType :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_industryType = Lens.lens (\Workload' {industryType} -> industryType) (\s@Workload' {} a -> s {industryType = a} :: Workload)

-- | Undocumented member.
workload_workloadId :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_workloadId = Lens.lens (\Workload' {workloadId} -> workloadId) (\s@Workload' {} a -> s {workloadId = a} :: Workload)

-- | Undocumented member.
workload_workloadName :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_workloadName = Lens.lens (\Workload' {workloadName} -> workloadName) (\s@Workload' {} a -> s {workloadName = a} :: Workload)

-- | Undocumented member.
workload_updatedAt :: Lens.Lens' Workload (Prelude.Maybe Prelude.UTCTime)
workload_updatedAt = Lens.lens (\Workload' {updatedAt} -> updatedAt) (\s@Workload' {} a -> s {updatedAt = a} :: Workload) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
workload_notes :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_notes = Lens.lens (\Workload' {notes} -> notes) (\s@Workload' {} a -> s {notes = a} :: Workload)

-- | Undocumented member.
workload_reviewOwner :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_reviewOwner = Lens.lens (\Workload' {reviewOwner} -> reviewOwner) (\s@Workload' {} a -> s {reviewOwner = a} :: Workload)

-- | Undocumented member.
workload_description :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_description = Lens.lens (\Workload' {description} -> description) (\s@Workload' {} a -> s {description = a} :: Workload)

-- | Undocumented member.
workload_pillarPriorities :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_pillarPriorities = Lens.lens (\Workload' {pillarPriorities} -> pillarPriorities) (\s@Workload' {} a -> s {pillarPriorities = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | The ID assigned to the share invitation.
workload_shareInvitationId :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_shareInvitationId = Lens.lens (\Workload' {shareInvitationId} -> shareInvitationId) (\s@Workload' {} a -> s {shareInvitationId = a} :: Workload)

-- | Undocumented member.
workload_nonAwsRegions :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_nonAwsRegions = Lens.lens (\Workload' {nonAwsRegions} -> nonAwsRegions) (\s@Workload' {} a -> s {nonAwsRegions = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | The tags associated with the workload.
workload_tags :: Lens.Lens' Workload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workload_tags = Lens.lens (\Workload' {tags} -> tags) (\s@Workload' {} a -> s {tags = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Workload where
  parseJSON =
    Core.withObject
      "Workload"
      ( \x ->
          Workload'
            Prelude.<$> (x Core..:? "IsReviewOwnerUpdateAcknowledged")
            Prelude.<*> (x Core..:? "ArchitecturalDesign")
            Prelude.<*> (x Core..:? "AccountIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Lenses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReviewRestrictionDate")
            Prelude.<*> (x Core..:? "Industry")
            Prelude.<*> (x Core..:? "Environment")
            Prelude.<*> (x Core..:? "RiskCounts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AwsRegions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "ImprovementStatus")
            Prelude.<*> (x Core..:? "WorkloadArn")
            Prelude.<*> (x Core..:? "IndustryType")
            Prelude.<*> (x Core..:? "WorkloadId")
            Prelude.<*> (x Core..:? "WorkloadName")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "Notes")
            Prelude.<*> (x Core..:? "ReviewOwner")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> ( x Core..:? "PillarPriorities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ShareInvitationId")
            Prelude.<*> (x Core..:? "NonAwsRegions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Workload

instance Prelude.NFData Workload
