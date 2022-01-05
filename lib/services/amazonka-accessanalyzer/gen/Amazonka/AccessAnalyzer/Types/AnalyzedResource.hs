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
-- Module      : Amazonka.AccessAnalyzer.Types.AnalyzedResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AnalyzedResource where

import Amazonka.AccessAnalyzer.Types.FindingStatus
import Amazonka.AccessAnalyzer.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the analyzed resource.
--
-- /See:/ 'newAnalyzedResource' smart constructor.
data AnalyzedResource = AnalyzedResource'
  { -- | The current status of the finding generated from the analyzed resource.
    status :: Prelude.Maybe FindingStatus,
    -- | The actions that an external principal is granted permission to use by
    -- the policy that generated the finding.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | An error message.
    error :: Prelude.Maybe Prelude.Text,
    -- | Indicates how the access that generated the finding is granted. This is
    -- populated for Amazon S3 bucket findings.
    sharedVia :: Prelude.Maybe [Prelude.Text],
    -- | The time at which the resource was analyzed.
    analyzedAt :: Core.POSIX,
    -- | The time at which the finding was created.
    createdAt :: Core.POSIX,
    -- | Indicates whether the policy that generated the finding grants public
    -- access to the resource.
    isPublic :: Prelude.Bool,
    -- | The ARN of the resource that was analyzed.
    resourceArn :: Prelude.Text,
    -- | The Amazon Web Services account ID that owns the resource.
    resourceOwnerAccount :: Prelude.Text,
    -- | The type of the resource that was analyzed.
    resourceType :: ResourceType,
    -- | The time at which the finding was updated.
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'analyzedResource_status' - The current status of the finding generated from the analyzed resource.
--
-- 'actions', 'analyzedResource_actions' - The actions that an external principal is granted permission to use by
-- the policy that generated the finding.
--
-- 'error', 'analyzedResource_error' - An error message.
--
-- 'sharedVia', 'analyzedResource_sharedVia' - Indicates how the access that generated the finding is granted. This is
-- populated for Amazon S3 bucket findings.
--
-- 'analyzedAt', 'analyzedResource_analyzedAt' - The time at which the resource was analyzed.
--
-- 'createdAt', 'analyzedResource_createdAt' - The time at which the finding was created.
--
-- 'isPublic', 'analyzedResource_isPublic' - Indicates whether the policy that generated the finding grants public
-- access to the resource.
--
-- 'resourceArn', 'analyzedResource_resourceArn' - The ARN of the resource that was analyzed.
--
-- 'resourceOwnerAccount', 'analyzedResource_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource.
--
-- 'resourceType', 'analyzedResource_resourceType' - The type of the resource that was analyzed.
--
-- 'updatedAt', 'analyzedResource_updatedAt' - The time at which the finding was updated.
newAnalyzedResource ::
  -- | 'analyzedAt'
  Prelude.UTCTime ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'isPublic'
  Prelude.Bool ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourceOwnerAccount'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  AnalyzedResource
newAnalyzedResource
  pAnalyzedAt_
  pCreatedAt_
  pIsPublic_
  pResourceArn_
  pResourceOwnerAccount_
  pResourceType_
  pUpdatedAt_ =
    AnalyzedResource'
      { status = Prelude.Nothing,
        actions = Prelude.Nothing,
        error = Prelude.Nothing,
        sharedVia = Prelude.Nothing,
        analyzedAt = Core._Time Lens.# pAnalyzedAt_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        isPublic = pIsPublic_,
        resourceArn = pResourceArn_,
        resourceOwnerAccount = pResourceOwnerAccount_,
        resourceType = pResourceType_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | The current status of the finding generated from the analyzed resource.
analyzedResource_status :: Lens.Lens' AnalyzedResource (Prelude.Maybe FindingStatus)
analyzedResource_status = Lens.lens (\AnalyzedResource' {status} -> status) (\s@AnalyzedResource' {} a -> s {status = a} :: AnalyzedResource)

-- | The actions that an external principal is granted permission to use by
-- the policy that generated the finding.
analyzedResource_actions :: Lens.Lens' AnalyzedResource (Prelude.Maybe [Prelude.Text])
analyzedResource_actions = Lens.lens (\AnalyzedResource' {actions} -> actions) (\s@AnalyzedResource' {} a -> s {actions = a} :: AnalyzedResource) Prelude.. Lens.mapping Lens.coerced

-- | An error message.
analyzedResource_error :: Lens.Lens' AnalyzedResource (Prelude.Maybe Prelude.Text)
analyzedResource_error = Lens.lens (\AnalyzedResource' {error} -> error) (\s@AnalyzedResource' {} a -> s {error = a} :: AnalyzedResource)

-- | Indicates how the access that generated the finding is granted. This is
-- populated for Amazon S3 bucket findings.
analyzedResource_sharedVia :: Lens.Lens' AnalyzedResource (Prelude.Maybe [Prelude.Text])
analyzedResource_sharedVia = Lens.lens (\AnalyzedResource' {sharedVia} -> sharedVia) (\s@AnalyzedResource' {} a -> s {sharedVia = a} :: AnalyzedResource) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the resource was analyzed.
analyzedResource_analyzedAt :: Lens.Lens' AnalyzedResource Prelude.UTCTime
analyzedResource_analyzedAt = Lens.lens (\AnalyzedResource' {analyzedAt} -> analyzedAt) (\s@AnalyzedResource' {} a -> s {analyzedAt = a} :: AnalyzedResource) Prelude.. Core._Time

-- | The time at which the finding was created.
analyzedResource_createdAt :: Lens.Lens' AnalyzedResource Prelude.UTCTime
analyzedResource_createdAt = Lens.lens (\AnalyzedResource' {createdAt} -> createdAt) (\s@AnalyzedResource' {} a -> s {createdAt = a} :: AnalyzedResource) Prelude.. Core._Time

-- | Indicates whether the policy that generated the finding grants public
-- access to the resource.
analyzedResource_isPublic :: Lens.Lens' AnalyzedResource Prelude.Bool
analyzedResource_isPublic = Lens.lens (\AnalyzedResource' {isPublic} -> isPublic) (\s@AnalyzedResource' {} a -> s {isPublic = a} :: AnalyzedResource)

-- | The ARN of the resource that was analyzed.
analyzedResource_resourceArn :: Lens.Lens' AnalyzedResource Prelude.Text
analyzedResource_resourceArn = Lens.lens (\AnalyzedResource' {resourceArn} -> resourceArn) (\s@AnalyzedResource' {} a -> s {resourceArn = a} :: AnalyzedResource)

-- | The Amazon Web Services account ID that owns the resource.
analyzedResource_resourceOwnerAccount :: Lens.Lens' AnalyzedResource Prelude.Text
analyzedResource_resourceOwnerAccount = Lens.lens (\AnalyzedResource' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@AnalyzedResource' {} a -> s {resourceOwnerAccount = a} :: AnalyzedResource)

-- | The type of the resource that was analyzed.
analyzedResource_resourceType :: Lens.Lens' AnalyzedResource ResourceType
analyzedResource_resourceType = Lens.lens (\AnalyzedResource' {resourceType} -> resourceType) (\s@AnalyzedResource' {} a -> s {resourceType = a} :: AnalyzedResource)

-- | The time at which the finding was updated.
analyzedResource_updatedAt :: Lens.Lens' AnalyzedResource Prelude.UTCTime
analyzedResource_updatedAt = Lens.lens (\AnalyzedResource' {updatedAt} -> updatedAt) (\s@AnalyzedResource' {} a -> s {updatedAt = a} :: AnalyzedResource) Prelude.. Core._Time

instance Core.FromJSON AnalyzedResource where
  parseJSON =
    Core.withObject
      "AnalyzedResource"
      ( \x ->
          AnalyzedResource'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "actions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "error")
            Prelude.<*> (x Core..:? "sharedVia" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "analyzedAt")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "isPublic")
            Prelude.<*> (x Core..: "resourceArn")
            Prelude.<*> (x Core..: "resourceOwnerAccount")
            Prelude.<*> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "updatedAt")
      )

instance Prelude.Hashable AnalyzedResource where
  hashWithSalt _salt AnalyzedResource' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` sharedVia
      `Prelude.hashWithSalt` analyzedAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` isPublic
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceOwnerAccount
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData AnalyzedResource where
  rnf AnalyzedResource' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf sharedVia
      `Prelude.seq` Prelude.rnf analyzedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf isPublic
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceOwnerAccount
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf updatedAt
