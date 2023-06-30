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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AnalyzedResource where

import Amazonka.AccessAnalyzer.Types.FindingStatus
import Amazonka.AccessAnalyzer.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the analyzed resource.
--
-- /See:/ 'newAnalyzedResource' smart constructor.
data AnalyzedResource = AnalyzedResource'
  { -- | The actions that an external principal is granted permission to use by
    -- the policy that generated the finding.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | An error message.
    error :: Prelude.Maybe Prelude.Text,
    -- | Indicates how the access that generated the finding is granted. This is
    -- populated for Amazon S3 bucket findings.
    sharedVia :: Prelude.Maybe [Prelude.Text],
    -- | The current status of the finding generated from the analyzed resource.
    status :: Prelude.Maybe FindingStatus,
    -- | The ARN of the resource that was analyzed.
    resourceArn :: Prelude.Text,
    -- | The type of the resource that was analyzed.
    resourceType :: ResourceType,
    -- | The time at which the finding was created.
    createdAt :: Data.ISO8601,
    -- | The time at which the resource was analyzed.
    analyzedAt :: Data.ISO8601,
    -- | The time at which the finding was updated.
    updatedAt :: Data.ISO8601,
    -- | Indicates whether the policy that generated the finding grants public
    -- access to the resource.
    isPublic :: Prelude.Bool,
    -- | The Amazon Web Services account ID that owns the resource.
    resourceOwnerAccount :: Prelude.Text
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
-- 'actions', 'analyzedResource_actions' - The actions that an external principal is granted permission to use by
-- the policy that generated the finding.
--
-- 'error', 'analyzedResource_error' - An error message.
--
-- 'sharedVia', 'analyzedResource_sharedVia' - Indicates how the access that generated the finding is granted. This is
-- populated for Amazon S3 bucket findings.
--
-- 'status', 'analyzedResource_status' - The current status of the finding generated from the analyzed resource.
--
-- 'resourceArn', 'analyzedResource_resourceArn' - The ARN of the resource that was analyzed.
--
-- 'resourceType', 'analyzedResource_resourceType' - The type of the resource that was analyzed.
--
-- 'createdAt', 'analyzedResource_createdAt' - The time at which the finding was created.
--
-- 'analyzedAt', 'analyzedResource_analyzedAt' - The time at which the resource was analyzed.
--
-- 'updatedAt', 'analyzedResource_updatedAt' - The time at which the finding was updated.
--
-- 'isPublic', 'analyzedResource_isPublic' - Indicates whether the policy that generated the finding grants public
-- access to the resource.
--
-- 'resourceOwnerAccount', 'analyzedResource_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource.
newAnalyzedResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'analyzedAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'isPublic'
  Prelude.Bool ->
  -- | 'resourceOwnerAccount'
  Prelude.Text ->
  AnalyzedResource
newAnalyzedResource
  pResourceArn_
  pResourceType_
  pCreatedAt_
  pAnalyzedAt_
  pUpdatedAt_
  pIsPublic_
  pResourceOwnerAccount_ =
    AnalyzedResource'
      { actions = Prelude.Nothing,
        error = Prelude.Nothing,
        sharedVia = Prelude.Nothing,
        status = Prelude.Nothing,
        resourceArn = pResourceArn_,
        resourceType = pResourceType_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        analyzedAt = Data._Time Lens.# pAnalyzedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        isPublic = pIsPublic_,
        resourceOwnerAccount = pResourceOwnerAccount_
      }

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

-- | The current status of the finding generated from the analyzed resource.
analyzedResource_status :: Lens.Lens' AnalyzedResource (Prelude.Maybe FindingStatus)
analyzedResource_status = Lens.lens (\AnalyzedResource' {status} -> status) (\s@AnalyzedResource' {} a -> s {status = a} :: AnalyzedResource)

-- | The ARN of the resource that was analyzed.
analyzedResource_resourceArn :: Lens.Lens' AnalyzedResource Prelude.Text
analyzedResource_resourceArn = Lens.lens (\AnalyzedResource' {resourceArn} -> resourceArn) (\s@AnalyzedResource' {} a -> s {resourceArn = a} :: AnalyzedResource)

-- | The type of the resource that was analyzed.
analyzedResource_resourceType :: Lens.Lens' AnalyzedResource ResourceType
analyzedResource_resourceType = Lens.lens (\AnalyzedResource' {resourceType} -> resourceType) (\s@AnalyzedResource' {} a -> s {resourceType = a} :: AnalyzedResource)

-- | The time at which the finding was created.
analyzedResource_createdAt :: Lens.Lens' AnalyzedResource Prelude.UTCTime
analyzedResource_createdAt = Lens.lens (\AnalyzedResource' {createdAt} -> createdAt) (\s@AnalyzedResource' {} a -> s {createdAt = a} :: AnalyzedResource) Prelude.. Data._Time

-- | The time at which the resource was analyzed.
analyzedResource_analyzedAt :: Lens.Lens' AnalyzedResource Prelude.UTCTime
analyzedResource_analyzedAt = Lens.lens (\AnalyzedResource' {analyzedAt} -> analyzedAt) (\s@AnalyzedResource' {} a -> s {analyzedAt = a} :: AnalyzedResource) Prelude.. Data._Time

-- | The time at which the finding was updated.
analyzedResource_updatedAt :: Lens.Lens' AnalyzedResource Prelude.UTCTime
analyzedResource_updatedAt = Lens.lens (\AnalyzedResource' {updatedAt} -> updatedAt) (\s@AnalyzedResource' {} a -> s {updatedAt = a} :: AnalyzedResource) Prelude.. Data._Time

-- | Indicates whether the policy that generated the finding grants public
-- access to the resource.
analyzedResource_isPublic :: Lens.Lens' AnalyzedResource Prelude.Bool
analyzedResource_isPublic = Lens.lens (\AnalyzedResource' {isPublic} -> isPublic) (\s@AnalyzedResource' {} a -> s {isPublic = a} :: AnalyzedResource)

-- | The Amazon Web Services account ID that owns the resource.
analyzedResource_resourceOwnerAccount :: Lens.Lens' AnalyzedResource Prelude.Text
analyzedResource_resourceOwnerAccount = Lens.lens (\AnalyzedResource' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@AnalyzedResource' {} a -> s {resourceOwnerAccount = a} :: AnalyzedResource)

instance Data.FromJSON AnalyzedResource where
  parseJSON =
    Data.withObject
      "AnalyzedResource"
      ( \x ->
          AnalyzedResource'
            Prelude.<$> (x Data..:? "actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..:? "sharedVia" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "resourceArn")
            Prelude.<*> (x Data..: "resourceType")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "analyzedAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "isPublic")
            Prelude.<*> (x Data..: "resourceOwnerAccount")
      )

instance Prelude.Hashable AnalyzedResource where
  hashWithSalt _salt AnalyzedResource' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` sharedVia
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` analyzedAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` isPublic
      `Prelude.hashWithSalt` resourceOwnerAccount

instance Prelude.NFData AnalyzedResource where
  rnf AnalyzedResource' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf sharedVia
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf analyzedAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf isPublic
      `Prelude.seq` Prelude.rnf resourceOwnerAccount
