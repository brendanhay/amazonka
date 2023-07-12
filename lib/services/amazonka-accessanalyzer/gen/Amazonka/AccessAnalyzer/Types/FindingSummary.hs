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
-- Module      : Amazonka.AccessAnalyzer.Types.FindingSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.FindingSummary where

import Amazonka.AccessAnalyzer.Types.FindingSource
import Amazonka.AccessAnalyzer.Types.FindingStatus
import Amazonka.AccessAnalyzer.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a finding.
--
-- /See:/ 'newFindingSummary' smart constructor.
data FindingSummary = FindingSummary'
  { -- | The action in the analyzed policy statement that an external principal
    -- has permission to use.
    action :: Prelude.Maybe [Prelude.Text],
    -- | The error that resulted in an Error finding.
    error :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the finding reports a resource that has a policy that
    -- allows public access.
    isPublic :: Prelude.Maybe Prelude.Bool,
    -- | The external principal that has access to a resource within the zone of
    -- trust.
    principal :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The resource that the external principal has access to.
    resource :: Prelude.Maybe Prelude.Text,
    -- | The sources of the finding. This indicates how the access that generated
    -- the finding is granted. It is populated for Amazon S3 bucket findings.
    sources :: Prelude.Maybe [FindingSource],
    -- | The ID of the finding.
    id :: Prelude.Text,
    -- | The type of the resource that the external principal has access to.
    resourceType :: ResourceType,
    -- | The condition in the analyzed policy statement that resulted in a
    -- finding.
    condition :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The time at which the finding was created.
    createdAt :: Data.ISO8601,
    -- | The time at which the resource-based policy that generated the finding
    -- was analyzed.
    analyzedAt :: Data.ISO8601,
    -- | The time at which the finding was most recently updated.
    updatedAt :: Data.ISO8601,
    -- | The status of the finding.
    status :: FindingStatus,
    -- | The Amazon Web Services account ID that owns the resource.
    resourceOwnerAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'findingSummary_action' - The action in the analyzed policy statement that an external principal
-- has permission to use.
--
-- 'error', 'findingSummary_error' - The error that resulted in an Error finding.
--
-- 'isPublic', 'findingSummary_isPublic' - Indicates whether the finding reports a resource that has a policy that
-- allows public access.
--
-- 'principal', 'findingSummary_principal' - The external principal that has access to a resource within the zone of
-- trust.
--
-- 'resource', 'findingSummary_resource' - The resource that the external principal has access to.
--
-- 'sources', 'findingSummary_sources' - The sources of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
--
-- 'id', 'findingSummary_id' - The ID of the finding.
--
-- 'resourceType', 'findingSummary_resourceType' - The type of the resource that the external principal has access to.
--
-- 'condition', 'findingSummary_condition' - The condition in the analyzed policy statement that resulted in a
-- finding.
--
-- 'createdAt', 'findingSummary_createdAt' - The time at which the finding was created.
--
-- 'analyzedAt', 'findingSummary_analyzedAt' - The time at which the resource-based policy that generated the finding
-- was analyzed.
--
-- 'updatedAt', 'findingSummary_updatedAt' - The time at which the finding was most recently updated.
--
-- 'status', 'findingSummary_status' - The status of the finding.
--
-- 'resourceOwnerAccount', 'findingSummary_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource.
newFindingSummary ::
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
  FindingSummary
newFindingSummary
  pId_
  pResourceType_
  pCreatedAt_
  pAnalyzedAt_
  pUpdatedAt_
  pStatus_
  pResourceOwnerAccount_ =
    FindingSummary'
      { action = Prelude.Nothing,
        error = Prelude.Nothing,
        isPublic = Prelude.Nothing,
        principal = Prelude.Nothing,
        resource = Prelude.Nothing,
        sources = Prelude.Nothing,
        id = pId_,
        resourceType = pResourceType_,
        condition = Prelude.mempty,
        createdAt = Data._Time Lens.# pCreatedAt_,
        analyzedAt = Data._Time Lens.# pAnalyzedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        status = pStatus_,
        resourceOwnerAccount = pResourceOwnerAccount_
      }

-- | The action in the analyzed policy statement that an external principal
-- has permission to use.
findingSummary_action :: Lens.Lens' FindingSummary (Prelude.Maybe [Prelude.Text])
findingSummary_action = Lens.lens (\FindingSummary' {action} -> action) (\s@FindingSummary' {} a -> s {action = a} :: FindingSummary) Prelude.. Lens.mapping Lens.coerced

-- | The error that resulted in an Error finding.
findingSummary_error :: Lens.Lens' FindingSummary (Prelude.Maybe Prelude.Text)
findingSummary_error = Lens.lens (\FindingSummary' {error} -> error) (\s@FindingSummary' {} a -> s {error = a} :: FindingSummary)

-- | Indicates whether the finding reports a resource that has a policy that
-- allows public access.
findingSummary_isPublic :: Lens.Lens' FindingSummary (Prelude.Maybe Prelude.Bool)
findingSummary_isPublic = Lens.lens (\FindingSummary' {isPublic} -> isPublic) (\s@FindingSummary' {} a -> s {isPublic = a} :: FindingSummary)

-- | The external principal that has access to a resource within the zone of
-- trust.
findingSummary_principal :: Lens.Lens' FindingSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
findingSummary_principal = Lens.lens (\FindingSummary' {principal} -> principal) (\s@FindingSummary' {} a -> s {principal = a} :: FindingSummary) Prelude.. Lens.mapping Lens.coerced

-- | The resource that the external principal has access to.
findingSummary_resource :: Lens.Lens' FindingSummary (Prelude.Maybe Prelude.Text)
findingSummary_resource = Lens.lens (\FindingSummary' {resource} -> resource) (\s@FindingSummary' {} a -> s {resource = a} :: FindingSummary)

-- | The sources of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
findingSummary_sources :: Lens.Lens' FindingSummary (Prelude.Maybe [FindingSource])
findingSummary_sources = Lens.lens (\FindingSummary' {sources} -> sources) (\s@FindingSummary' {} a -> s {sources = a} :: FindingSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the finding.
findingSummary_id :: Lens.Lens' FindingSummary Prelude.Text
findingSummary_id = Lens.lens (\FindingSummary' {id} -> id) (\s@FindingSummary' {} a -> s {id = a} :: FindingSummary)

-- | The type of the resource that the external principal has access to.
findingSummary_resourceType :: Lens.Lens' FindingSummary ResourceType
findingSummary_resourceType = Lens.lens (\FindingSummary' {resourceType} -> resourceType) (\s@FindingSummary' {} a -> s {resourceType = a} :: FindingSummary)

-- | The condition in the analyzed policy statement that resulted in a
-- finding.
findingSummary_condition :: Lens.Lens' FindingSummary (Prelude.HashMap Prelude.Text Prelude.Text)
findingSummary_condition = Lens.lens (\FindingSummary' {condition} -> condition) (\s@FindingSummary' {} a -> s {condition = a} :: FindingSummary) Prelude.. Lens.coerced

-- | The time at which the finding was created.
findingSummary_createdAt :: Lens.Lens' FindingSummary Prelude.UTCTime
findingSummary_createdAt = Lens.lens (\FindingSummary' {createdAt} -> createdAt) (\s@FindingSummary' {} a -> s {createdAt = a} :: FindingSummary) Prelude.. Data._Time

-- | The time at which the resource-based policy that generated the finding
-- was analyzed.
findingSummary_analyzedAt :: Lens.Lens' FindingSummary Prelude.UTCTime
findingSummary_analyzedAt = Lens.lens (\FindingSummary' {analyzedAt} -> analyzedAt) (\s@FindingSummary' {} a -> s {analyzedAt = a} :: FindingSummary) Prelude.. Data._Time

-- | The time at which the finding was most recently updated.
findingSummary_updatedAt :: Lens.Lens' FindingSummary Prelude.UTCTime
findingSummary_updatedAt = Lens.lens (\FindingSummary' {updatedAt} -> updatedAt) (\s@FindingSummary' {} a -> s {updatedAt = a} :: FindingSummary) Prelude.. Data._Time

-- | The status of the finding.
findingSummary_status :: Lens.Lens' FindingSummary FindingStatus
findingSummary_status = Lens.lens (\FindingSummary' {status} -> status) (\s@FindingSummary' {} a -> s {status = a} :: FindingSummary)

-- | The Amazon Web Services account ID that owns the resource.
findingSummary_resourceOwnerAccount :: Lens.Lens' FindingSummary Prelude.Text
findingSummary_resourceOwnerAccount = Lens.lens (\FindingSummary' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@FindingSummary' {} a -> s {resourceOwnerAccount = a} :: FindingSummary)

instance Data.FromJSON FindingSummary where
  parseJSON =
    Data.withObject
      "FindingSummary"
      ( \x ->
          FindingSummary'
            Prelude.<$> (x Data..:? "action" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..:? "isPublic")
            Prelude.<*> (x Data..:? "principal" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resource")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..:? "condition" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "analyzedAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "resourceOwnerAccount")
      )

instance Prelude.Hashable FindingSummary where
  hashWithSalt _salt FindingSummary' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` isPublic
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` analyzedAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceOwnerAccount

instance Prelude.NFData FindingSummary where
  rnf FindingSummary' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf isPublic
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf analyzedAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceOwnerAccount
