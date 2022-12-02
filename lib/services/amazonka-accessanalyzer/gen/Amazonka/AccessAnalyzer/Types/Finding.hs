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
-- Module      : Amazonka.AccessAnalyzer.Types.Finding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Finding where

import Amazonka.AccessAnalyzer.Types.FindingSource
import Amazonka.AccessAnalyzer.Types.FindingStatus
import Amazonka.AccessAnalyzer.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a finding.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | The external principal that access to a resource within the zone of
    -- trust.
    principal :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The sources of the finding. This indicates how the access that generated
    -- the finding is granted. It is populated for Amazon S3 bucket findings.
    sources :: Prelude.Maybe [FindingSource],
    -- | Indicates whether the policy that generated the finding allows public
    -- access to the resource.
    isPublic :: Prelude.Maybe Prelude.Bool,
    -- | The action in the analyzed policy statement that an external principal
    -- has permission to use.
    action :: Prelude.Maybe [Prelude.Text],
    -- | An error.
    error :: Prelude.Maybe Prelude.Text,
    -- | The resource that an external principal has access to.
    resource :: Prelude.Maybe Prelude.Text,
    -- | The ID of the finding.
    id :: Prelude.Text,
    -- | The type of the resource identified in the finding.
    resourceType :: ResourceType,
    -- | The condition in the analyzed policy statement that resulted in a
    -- finding.
    condition :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The time at which the finding was generated.
    createdAt :: Data.POSIX,
    -- | The time at which the resource was analyzed.
    analyzedAt :: Data.POSIX,
    -- | The time at which the finding was updated.
    updatedAt :: Data.POSIX,
    -- | The current status of the finding.
    status :: FindingStatus,
    -- | The Amazon Web Services account ID that owns the resource.
    resourceOwnerAccount :: Prelude.Text
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
-- 'principal', 'finding_principal' - The external principal that access to a resource within the zone of
-- trust.
--
-- 'sources', 'finding_sources' - The sources of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
--
-- 'isPublic', 'finding_isPublic' - Indicates whether the policy that generated the finding allows public
-- access to the resource.
--
-- 'action', 'finding_action' - The action in the analyzed policy statement that an external principal
-- has permission to use.
--
-- 'error', 'finding_error' - An error.
--
-- 'resource', 'finding_resource' - The resource that an external principal has access to.
--
-- 'id', 'finding_id' - The ID of the finding.
--
-- 'resourceType', 'finding_resourceType' - The type of the resource identified in the finding.
--
-- 'condition', 'finding_condition' - The condition in the analyzed policy statement that resulted in a
-- finding.
--
-- 'createdAt', 'finding_createdAt' - The time at which the finding was generated.
--
-- 'analyzedAt', 'finding_analyzedAt' - The time at which the resource was analyzed.
--
-- 'updatedAt', 'finding_updatedAt' - The time at which the finding was updated.
--
-- 'status', 'finding_status' - The current status of the finding.
--
-- 'resourceOwnerAccount', 'finding_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource.
newFinding ::
  -- | 'id'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'analyzedAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'status'
  FindingStatus ->
  -- | 'resourceOwnerAccount'
  Prelude.Text ->
  Finding
newFinding
  pId_
  pResourceType_
  pCreatedAt_
  pAnalyzedAt_
  pUpdatedAt_
  pStatus_
  pResourceOwnerAccount_ =
    Finding'
      { principal = Prelude.Nothing,
        sources = Prelude.Nothing,
        isPublic = Prelude.Nothing,
        action = Prelude.Nothing,
        error = Prelude.Nothing,
        resource = Prelude.Nothing,
        id = pId_,
        resourceType = pResourceType_,
        condition = Prelude.mempty,
        createdAt = Data._Time Lens.# pCreatedAt_,
        analyzedAt = Data._Time Lens.# pAnalyzedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        status = pStatus_,
        resourceOwnerAccount = pResourceOwnerAccount_
      }

-- | The external principal that access to a resource within the zone of
-- trust.
finding_principal :: Lens.Lens' Finding (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
finding_principal = Lens.lens (\Finding' {principal} -> principal) (\s@Finding' {} a -> s {principal = a} :: Finding) Prelude.. Lens.mapping Lens.coerced

-- | The sources of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
finding_sources :: Lens.Lens' Finding (Prelude.Maybe [FindingSource])
finding_sources = Lens.lens (\Finding' {sources} -> sources) (\s@Finding' {} a -> s {sources = a} :: Finding) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the policy that generated the finding allows public
-- access to the resource.
finding_isPublic :: Lens.Lens' Finding (Prelude.Maybe Prelude.Bool)
finding_isPublic = Lens.lens (\Finding' {isPublic} -> isPublic) (\s@Finding' {} a -> s {isPublic = a} :: Finding)

-- | The action in the analyzed policy statement that an external principal
-- has permission to use.
finding_action :: Lens.Lens' Finding (Prelude.Maybe [Prelude.Text])
finding_action = Lens.lens (\Finding' {action} -> action) (\s@Finding' {} a -> s {action = a} :: Finding) Prelude.. Lens.mapping Lens.coerced

-- | An error.
finding_error :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_error = Lens.lens (\Finding' {error} -> error) (\s@Finding' {} a -> s {error = a} :: Finding)

-- | The resource that an external principal has access to.
finding_resource :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_resource = Lens.lens (\Finding' {resource} -> resource) (\s@Finding' {} a -> s {resource = a} :: Finding)

-- | The ID of the finding.
finding_id :: Lens.Lens' Finding Prelude.Text
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | The type of the resource identified in the finding.
finding_resourceType :: Lens.Lens' Finding ResourceType
finding_resourceType = Lens.lens (\Finding' {resourceType} -> resourceType) (\s@Finding' {} a -> s {resourceType = a} :: Finding)

-- | The condition in the analyzed policy statement that resulted in a
-- finding.
finding_condition :: Lens.Lens' Finding (Prelude.HashMap Prelude.Text Prelude.Text)
finding_condition = Lens.lens (\Finding' {condition} -> condition) (\s@Finding' {} a -> s {condition = a} :: Finding) Prelude.. Lens.coerced

-- | The time at which the finding was generated.
finding_createdAt :: Lens.Lens' Finding Prelude.UTCTime
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding) Prelude.. Data._Time

-- | The time at which the resource was analyzed.
finding_analyzedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_analyzedAt = Lens.lens (\Finding' {analyzedAt} -> analyzedAt) (\s@Finding' {} a -> s {analyzedAt = a} :: Finding) Prelude.. Data._Time

-- | The time at which the finding was updated.
finding_updatedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Prelude.. Data._Time

-- | The current status of the finding.
finding_status :: Lens.Lens' Finding FindingStatus
finding_status = Lens.lens (\Finding' {status} -> status) (\s@Finding' {} a -> s {status = a} :: Finding)

-- | The Amazon Web Services account ID that owns the resource.
finding_resourceOwnerAccount :: Lens.Lens' Finding Prelude.Text
finding_resourceOwnerAccount = Lens.lens (\Finding' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@Finding' {} a -> s {resourceOwnerAccount = a} :: Finding)

instance Data.FromJSON Finding where
  parseJSON =
    Data.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Data..:? "principal" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "isPublic")
            Prelude.<*> (x Data..:? "action" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..:? "resource")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..:? "condition" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "analyzedAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "resourceOwnerAccount")
      )

instance Prelude.Hashable Finding where
  hashWithSalt _salt Finding' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` isPublic
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` analyzedAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceOwnerAccount

instance Prelude.NFData Finding where
  rnf Finding' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf isPublic
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf analyzedAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceOwnerAccount
