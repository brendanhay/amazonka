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
-- Module      : Network.AWS.AccessAnalyzer.Types.Finding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.Finding where

import Network.AWS.AccessAnalyzer.Types.FindingSource
import Network.AWS.AccessAnalyzer.Types.FindingStatus
import Network.AWS.AccessAnalyzer.Types.ResourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a finding.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | An error.
    error :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the policy that generated the finding allows public
    -- access to the resource.
    isPublic :: Prelude.Maybe Prelude.Bool,
    -- | The action in the analyzed policy statement that an external principal
    -- has permission to use.
    action :: Prelude.Maybe [Prelude.Text],
    -- | The sources of the finding. This indicates how the access that generated
    -- the finding is granted. It is populated for Amazon S3 bucket findings.
    sources :: Prelude.Maybe [FindingSource],
    -- | The resource that an external principal has access to.
    resource :: Prelude.Maybe Prelude.Text,
    -- | The external principal that access to a resource within the zone of
    -- trust.
    principal :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time at which the resource was analyzed.
    analyzedAt :: Core.POSIX,
    -- | The condition in the analyzed policy statement that resulted in a
    -- finding.
    condition :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The time at which the finding was generated.
    createdAt :: Core.POSIX,
    -- | The ID of the finding.
    id :: Prelude.Text,
    -- | The Amazon Web Services account ID that owns the resource.
    resourceOwnerAccount :: Prelude.Text,
    -- | The type of the resource identified in the finding.
    resourceType :: ResourceType,
    -- | The current status of the finding.
    status :: FindingStatus,
    -- | The time at which the finding was updated.
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Finding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'finding_error' - An error.
--
-- 'isPublic', 'finding_isPublic' - Indicates whether the policy that generated the finding allows public
-- access to the resource.
--
-- 'action', 'finding_action' - The action in the analyzed policy statement that an external principal
-- has permission to use.
--
-- 'sources', 'finding_sources' - The sources of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
--
-- 'resource', 'finding_resource' - The resource that an external principal has access to.
--
-- 'principal', 'finding_principal' - The external principal that access to a resource within the zone of
-- trust.
--
-- 'analyzedAt', 'finding_analyzedAt' - The time at which the resource was analyzed.
--
-- 'condition', 'finding_condition' - The condition in the analyzed policy statement that resulted in a
-- finding.
--
-- 'createdAt', 'finding_createdAt' - The time at which the finding was generated.
--
-- 'id', 'finding_id' - The ID of the finding.
--
-- 'resourceOwnerAccount', 'finding_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource.
--
-- 'resourceType', 'finding_resourceType' - The type of the resource identified in the finding.
--
-- 'status', 'finding_status' - The current status of the finding.
--
-- 'updatedAt', 'finding_updatedAt' - The time at which the finding was updated.
newFinding ::
  -- | 'analyzedAt'
  Prelude.UTCTime ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'resourceOwnerAccount'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'status'
  FindingStatus ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  Finding
newFinding
  pAnalyzedAt_
  pCreatedAt_
  pId_
  pResourceOwnerAccount_
  pResourceType_
  pStatus_
  pUpdatedAt_ =
    Finding'
      { error = Prelude.Nothing,
        isPublic = Prelude.Nothing,
        action = Prelude.Nothing,
        sources = Prelude.Nothing,
        resource = Prelude.Nothing,
        principal = Prelude.Nothing,
        analyzedAt = Core._Time Lens.# pAnalyzedAt_,
        condition = Prelude.mempty,
        createdAt = Core._Time Lens.# pCreatedAt_,
        id = pId_,
        resourceOwnerAccount = pResourceOwnerAccount_,
        resourceType = pResourceType_,
        status = pStatus_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | An error.
finding_error :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_error = Lens.lens (\Finding' {error} -> error) (\s@Finding' {} a -> s {error = a} :: Finding)

-- | Indicates whether the policy that generated the finding allows public
-- access to the resource.
finding_isPublic :: Lens.Lens' Finding (Prelude.Maybe Prelude.Bool)
finding_isPublic = Lens.lens (\Finding' {isPublic} -> isPublic) (\s@Finding' {} a -> s {isPublic = a} :: Finding)

-- | The action in the analyzed policy statement that an external principal
-- has permission to use.
finding_action :: Lens.Lens' Finding (Prelude.Maybe [Prelude.Text])
finding_action = Lens.lens (\Finding' {action} -> action) (\s@Finding' {} a -> s {action = a} :: Finding) Prelude.. Lens.mapping Lens.coerced

-- | The sources of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
finding_sources :: Lens.Lens' Finding (Prelude.Maybe [FindingSource])
finding_sources = Lens.lens (\Finding' {sources} -> sources) (\s@Finding' {} a -> s {sources = a} :: Finding) Prelude.. Lens.mapping Lens.coerced

-- | The resource that an external principal has access to.
finding_resource :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_resource = Lens.lens (\Finding' {resource} -> resource) (\s@Finding' {} a -> s {resource = a} :: Finding)

-- | The external principal that access to a resource within the zone of
-- trust.
finding_principal :: Lens.Lens' Finding (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
finding_principal = Lens.lens (\Finding' {principal} -> principal) (\s@Finding' {} a -> s {principal = a} :: Finding) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the resource was analyzed.
finding_analyzedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_analyzedAt = Lens.lens (\Finding' {analyzedAt} -> analyzedAt) (\s@Finding' {} a -> s {analyzedAt = a} :: Finding) Prelude.. Core._Time

-- | The condition in the analyzed policy statement that resulted in a
-- finding.
finding_condition :: Lens.Lens' Finding (Prelude.HashMap Prelude.Text Prelude.Text)
finding_condition = Lens.lens (\Finding' {condition} -> condition) (\s@Finding' {} a -> s {condition = a} :: Finding) Prelude.. Lens.coerced

-- | The time at which the finding was generated.
finding_createdAt :: Lens.Lens' Finding Prelude.UTCTime
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding) Prelude.. Core._Time

-- | The ID of the finding.
finding_id :: Lens.Lens' Finding Prelude.Text
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | The Amazon Web Services account ID that owns the resource.
finding_resourceOwnerAccount :: Lens.Lens' Finding Prelude.Text
finding_resourceOwnerAccount = Lens.lens (\Finding' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@Finding' {} a -> s {resourceOwnerAccount = a} :: Finding)

-- | The type of the resource identified in the finding.
finding_resourceType :: Lens.Lens' Finding ResourceType
finding_resourceType = Lens.lens (\Finding' {resourceType} -> resourceType) (\s@Finding' {} a -> s {resourceType = a} :: Finding)

-- | The current status of the finding.
finding_status :: Lens.Lens' Finding FindingStatus
finding_status = Lens.lens (\Finding' {status} -> status) (\s@Finding' {} a -> s {status = a} :: Finding)

-- | The time at which the finding was updated.
finding_updatedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Prelude.. Core._Time

instance Core.FromJSON Finding where
  parseJSON =
    Core.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "isPublic")
            Prelude.<*> (x Core..:? "action" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "sources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resource")
            Prelude.<*> (x Core..:? "principal" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "analyzedAt")
            Prelude.<*> (x Core..:? "condition" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "resourceOwnerAccount")
            Prelude.<*> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "updatedAt")
      )

instance Prelude.Hashable Finding

instance Prelude.NFData Finding
