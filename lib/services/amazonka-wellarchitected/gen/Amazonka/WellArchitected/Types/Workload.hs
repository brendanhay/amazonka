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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Workload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.Risk
import Amazonka.WellArchitected.Types.WorkloadDiscoveryConfig
import Amazonka.WellArchitected.Types.WorkloadEnvironment
import Amazonka.WellArchitected.Types.WorkloadImprovementStatus

-- | A workload return object.
--
-- /See:/ 'newWorkload' smart constructor.
data Workload = Workload'
  { -- | Discovery configuration associated to the workload.
    discoveryConfig :: Prelude.Maybe WorkloadDiscoveryConfig,
    -- | The tags associated with the workload.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    accountIds :: Prelude.Maybe [Prelude.Text],
    environment :: Prelude.Maybe WorkloadEnvironment,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    -- | Flag indicating whether the workload owner has acknowledged that the
    -- /Review owner/ field is required.
    --
    -- If a __Review owner__ is not added to the workload within 60 days of
    -- acknowledgement, access to the workload is restricted until an owner is
    -- added.
    isReviewOwnerUpdateAcknowledged :: Prelude.Maybe Prelude.Bool,
    industry :: Prelude.Maybe Prelude.Text,
    -- | List of AppRegistry application ARNs associated to the workload.
    applications :: Prelude.Maybe [Prelude.Text],
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text,
    workloadArn :: Prelude.Maybe Prelude.Text,
    awsRegions :: Prelude.Maybe [Prelude.Text],
    workloadName :: Prelude.Maybe Prelude.Text,
    reviewOwner :: Prelude.Maybe Prelude.Text,
    owner :: Prelude.Maybe Prelude.Text,
    nonAwsRegions :: Prelude.Maybe [Prelude.Text],
    description :: Prelude.Maybe Prelude.Text,
    notes :: Prelude.Maybe Prelude.Text,
    industryType :: Prelude.Maybe Prelude.Text,
    architecturalDesign :: Prelude.Maybe Prelude.Text,
    pillarPriorities :: Prelude.Maybe [Prelude.Text],
    improvementStatus :: Prelude.Maybe WorkloadImprovementStatus,
    lenses :: Prelude.Maybe [Prelude.Text],
    reviewRestrictionDate :: Prelude.Maybe Core.POSIX,
    updatedAt :: Prelude.Maybe Core.POSIX,
    workloadId :: Prelude.Maybe Prelude.Text
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
-- 'discoveryConfig', 'workload_discoveryConfig' - Discovery configuration associated to the workload.
--
-- 'tags', 'workload_tags' - The tags associated with the workload.
--
-- 'accountIds', 'workload_accountIds' - Undocumented member.
--
-- 'environment', 'workload_environment' - Undocumented member.
--
-- 'riskCounts', 'workload_riskCounts' - Undocumented member.
--
-- 'isReviewOwnerUpdateAcknowledged', 'workload_isReviewOwnerUpdateAcknowledged' - Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
--
-- 'industry', 'workload_industry' - Undocumented member.
--
-- 'applications', 'workload_applications' - List of AppRegistry application ARNs associated to the workload.
--
-- 'shareInvitationId', 'workload_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'workloadArn', 'workload_workloadArn' - Undocumented member.
--
-- 'awsRegions', 'workload_awsRegions' - Undocumented member.
--
-- 'workloadName', 'workload_workloadName' - Undocumented member.
--
-- 'reviewOwner', 'workload_reviewOwner' - Undocumented member.
--
-- 'owner', 'workload_owner' - Undocumented member.
--
-- 'nonAwsRegions', 'workload_nonAwsRegions' - Undocumented member.
--
-- 'description', 'workload_description' - Undocumented member.
--
-- 'notes', 'workload_notes' - Undocumented member.
--
-- 'industryType', 'workload_industryType' - Undocumented member.
--
-- 'architecturalDesign', 'workload_architecturalDesign' - Undocumented member.
--
-- 'pillarPriorities', 'workload_pillarPriorities' - Undocumented member.
--
-- 'improvementStatus', 'workload_improvementStatus' - Undocumented member.
--
-- 'lenses', 'workload_lenses' - Undocumented member.
--
-- 'reviewRestrictionDate', 'workload_reviewRestrictionDate' - Undocumented member.
--
-- 'updatedAt', 'workload_updatedAt' - Undocumented member.
--
-- 'workloadId', 'workload_workloadId' - Undocumented member.
newWorkload ::
  Workload
newWorkload =
  Workload'
    { discoveryConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      accountIds = Prelude.Nothing,
      environment = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      isReviewOwnerUpdateAcknowledged = Prelude.Nothing,
      industry = Prelude.Nothing,
      applications = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing,
      workloadArn = Prelude.Nothing,
      awsRegions = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      reviewOwner = Prelude.Nothing,
      owner = Prelude.Nothing,
      nonAwsRegions = Prelude.Nothing,
      description = Prelude.Nothing,
      notes = Prelude.Nothing,
      industryType = Prelude.Nothing,
      architecturalDesign = Prelude.Nothing,
      pillarPriorities = Prelude.Nothing,
      improvementStatus = Prelude.Nothing,
      lenses = Prelude.Nothing,
      reviewRestrictionDate = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | Discovery configuration associated to the workload.
workload_discoveryConfig :: Lens.Lens' Workload (Prelude.Maybe WorkloadDiscoveryConfig)
workload_discoveryConfig = Lens.lens (\Workload' {discoveryConfig} -> discoveryConfig) (\s@Workload' {} a -> s {discoveryConfig = a} :: Workload)

-- | The tags associated with the workload.
workload_tags :: Lens.Lens' Workload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workload_tags = Lens.lens (\Workload' {tags} -> tags) (\s@Workload' {} a -> s {tags = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_accountIds :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_accountIds = Lens.lens (\Workload' {accountIds} -> accountIds) (\s@Workload' {} a -> s {accountIds = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_environment :: Lens.Lens' Workload (Prelude.Maybe WorkloadEnvironment)
workload_environment = Lens.lens (\Workload' {environment} -> environment) (\s@Workload' {} a -> s {environment = a} :: Workload)

-- | Undocumented member.
workload_riskCounts :: Lens.Lens' Workload (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
workload_riskCounts = Lens.lens (\Workload' {riskCounts} -> riskCounts) (\s@Workload' {} a -> s {riskCounts = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Flag indicating whether the workload owner has acknowledged that the
-- /Review owner/ field is required.
--
-- If a __Review owner__ is not added to the workload within 60 days of
-- acknowledgement, access to the workload is restricted until an owner is
-- added.
workload_isReviewOwnerUpdateAcknowledged :: Lens.Lens' Workload (Prelude.Maybe Prelude.Bool)
workload_isReviewOwnerUpdateAcknowledged = Lens.lens (\Workload' {isReviewOwnerUpdateAcknowledged} -> isReviewOwnerUpdateAcknowledged) (\s@Workload' {} a -> s {isReviewOwnerUpdateAcknowledged = a} :: Workload)

-- | Undocumented member.
workload_industry :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_industry = Lens.lens (\Workload' {industry} -> industry) (\s@Workload' {} a -> s {industry = a} :: Workload)

-- | List of AppRegistry application ARNs associated to the workload.
workload_applications :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_applications = Lens.lens (\Workload' {applications} -> applications) (\s@Workload' {} a -> s {applications = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | The ID assigned to the share invitation.
workload_shareInvitationId :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_shareInvitationId = Lens.lens (\Workload' {shareInvitationId} -> shareInvitationId) (\s@Workload' {} a -> s {shareInvitationId = a} :: Workload)

-- | Undocumented member.
workload_workloadArn :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_workloadArn = Lens.lens (\Workload' {workloadArn} -> workloadArn) (\s@Workload' {} a -> s {workloadArn = a} :: Workload)

-- | Undocumented member.
workload_awsRegions :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_awsRegions = Lens.lens (\Workload' {awsRegions} -> awsRegions) (\s@Workload' {} a -> s {awsRegions = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_workloadName :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_workloadName = Lens.lens (\Workload' {workloadName} -> workloadName) (\s@Workload' {} a -> s {workloadName = a} :: Workload)

-- | Undocumented member.
workload_reviewOwner :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_reviewOwner = Lens.lens (\Workload' {reviewOwner} -> reviewOwner) (\s@Workload' {} a -> s {reviewOwner = a} :: Workload)

-- | Undocumented member.
workload_owner :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_owner = Lens.lens (\Workload' {owner} -> owner) (\s@Workload' {} a -> s {owner = a} :: Workload)

-- | Undocumented member.
workload_nonAwsRegions :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_nonAwsRegions = Lens.lens (\Workload' {nonAwsRegions} -> nonAwsRegions) (\s@Workload' {} a -> s {nonAwsRegions = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_description :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_description = Lens.lens (\Workload' {description} -> description) (\s@Workload' {} a -> s {description = a} :: Workload)

-- | Undocumented member.
workload_notes :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_notes = Lens.lens (\Workload' {notes} -> notes) (\s@Workload' {} a -> s {notes = a} :: Workload)

-- | Undocumented member.
workload_industryType :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_industryType = Lens.lens (\Workload' {industryType} -> industryType) (\s@Workload' {} a -> s {industryType = a} :: Workload)

-- | Undocumented member.
workload_architecturalDesign :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_architecturalDesign = Lens.lens (\Workload' {architecturalDesign} -> architecturalDesign) (\s@Workload' {} a -> s {architecturalDesign = a} :: Workload)

-- | Undocumented member.
workload_pillarPriorities :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_pillarPriorities = Lens.lens (\Workload' {pillarPriorities} -> pillarPriorities) (\s@Workload' {} a -> s {pillarPriorities = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_improvementStatus :: Lens.Lens' Workload (Prelude.Maybe WorkloadImprovementStatus)
workload_improvementStatus = Lens.lens (\Workload' {improvementStatus} -> improvementStatus) (\s@Workload' {} a -> s {improvementStatus = a} :: Workload)

-- | Undocumented member.
workload_lenses :: Lens.Lens' Workload (Prelude.Maybe [Prelude.Text])
workload_lenses = Lens.lens (\Workload' {lenses} -> lenses) (\s@Workload' {} a -> s {lenses = a} :: Workload) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
workload_reviewRestrictionDate :: Lens.Lens' Workload (Prelude.Maybe Prelude.UTCTime)
workload_reviewRestrictionDate = Lens.lens (\Workload' {reviewRestrictionDate} -> reviewRestrictionDate) (\s@Workload' {} a -> s {reviewRestrictionDate = a} :: Workload) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
workload_updatedAt :: Lens.Lens' Workload (Prelude.Maybe Prelude.UTCTime)
workload_updatedAt = Lens.lens (\Workload' {updatedAt} -> updatedAt) (\s@Workload' {} a -> s {updatedAt = a} :: Workload) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
workload_workloadId :: Lens.Lens' Workload (Prelude.Maybe Prelude.Text)
workload_workloadId = Lens.lens (\Workload' {workloadId} -> workloadId) (\s@Workload' {} a -> s {workloadId = a} :: Workload)

instance Core.FromJSON Workload where
  parseJSON =
    Core.withObject
      "Workload"
      ( \x ->
          Workload'
            Prelude.<$> (x Core..:? "DiscoveryConfig")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AccountIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Environment")
            Prelude.<*> (x Core..:? "RiskCounts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IsReviewOwnerUpdateAcknowledged")
            Prelude.<*> (x Core..:? "Industry")
            Prelude.<*> (x Core..:? "Applications" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ShareInvitationId")
            Prelude.<*> (x Core..:? "WorkloadArn")
            Prelude.<*> (x Core..:? "AwsRegions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "WorkloadName")
            Prelude.<*> (x Core..:? "ReviewOwner")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "NonAwsRegions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Notes")
            Prelude.<*> (x Core..:? "IndustryType")
            Prelude.<*> (x Core..:? "ArchitecturalDesign")
            Prelude.<*> ( x Core..:? "PillarPriorities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ImprovementStatus")
            Prelude.<*> (x Core..:? "Lenses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReviewRestrictionDate")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "WorkloadId")
      )

instance Prelude.Hashable Workload where
  hashWithSalt _salt Workload' {..} =
    _salt `Prelude.hashWithSalt` discoveryConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` isReviewOwnerUpdateAcknowledged
      `Prelude.hashWithSalt` industry
      `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` workloadArn
      `Prelude.hashWithSalt` awsRegions
      `Prelude.hashWithSalt` workloadName
      `Prelude.hashWithSalt` reviewOwner
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` nonAwsRegions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` industryType
      `Prelude.hashWithSalt` architecturalDesign
      `Prelude.hashWithSalt` pillarPriorities
      `Prelude.hashWithSalt` improvementStatus
      `Prelude.hashWithSalt` lenses
      `Prelude.hashWithSalt` reviewRestrictionDate
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData Workload where
  rnf Workload' {..} =
    Prelude.rnf discoveryConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf isReviewOwnerUpdateAcknowledged
      `Prelude.seq` Prelude.rnf industry
      `Prelude.seq` Prelude.rnf applications
      `Prelude.seq` Prelude.rnf shareInvitationId
      `Prelude.seq` Prelude.rnf workloadArn
      `Prelude.seq` Prelude.rnf awsRegions
      `Prelude.seq` Prelude.rnf workloadName
      `Prelude.seq` Prelude.rnf reviewOwner
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf nonAwsRegions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf industryType
      `Prelude.seq` Prelude.rnf architecturalDesign
      `Prelude.seq` Prelude.rnf pillarPriorities
      `Prelude.seq` Prelude.rnf
        improvementStatus
      `Prelude.seq` Prelude.rnf lenses
      `Prelude.seq` Prelude.rnf
        reviewRestrictionDate
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf
        workloadId
