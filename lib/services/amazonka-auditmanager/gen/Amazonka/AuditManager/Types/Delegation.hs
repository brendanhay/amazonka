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
-- Module      : Amazonka.AuditManager.Types.Delegation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Delegation where

import Amazonka.AuditManager.Types.DelegationStatus
import Amazonka.AuditManager.Types.RoleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The assignment of a control set to a delegate for review.
--
-- /See:/ 'newDelegation' smart constructor.
data Delegation = Delegation'
  { -- | The type of customer persona.
    --
    -- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
    -- @RESOURCE_OWNER@.
    roleType :: Prelude.Maybe RoleType,
    -- | Specifies when the delegation was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the delegation.
    status :: Prelude.Maybe DelegationStatus,
    -- | Specifies when the delegation was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The identifier for the associated control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The IAM user or role that created the delegation.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the delegation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the associated assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The comment related to the delegation.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated assessment.
    assessmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Delegation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleType', 'delegation_roleType' - The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
--
-- 'creationTime', 'delegation_creationTime' - Specifies when the delegation was created.
--
-- 'status', 'delegation_status' - The status of the delegation.
--
-- 'lastUpdated', 'delegation_lastUpdated' - Specifies when the delegation was last updated.
--
-- 'controlSetId', 'delegation_controlSetId' - The identifier for the associated control set.
--
-- 'createdBy', 'delegation_createdBy' - The IAM user or role that created the delegation.
--
-- 'id', 'delegation_id' - The unique identifier for the delegation.
--
-- 'assessmentId', 'delegation_assessmentId' - The identifier for the associated assessment.
--
-- 'comment', 'delegation_comment' - The comment related to the delegation.
--
-- 'roleArn', 'delegation_roleArn' - The Amazon Resource Name (ARN) of the IAM role.
--
-- 'assessmentName', 'delegation_assessmentName' - The name of the associated assessment.
newDelegation ::
  Delegation
newDelegation =
  Delegation'
    { roleType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      id = Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      comment = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      assessmentName = Prelude.Nothing
    }

-- | The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
delegation_roleType :: Lens.Lens' Delegation (Prelude.Maybe RoleType)
delegation_roleType = Lens.lens (\Delegation' {roleType} -> roleType) (\s@Delegation' {} a -> s {roleType = a} :: Delegation)

-- | Specifies when the delegation was created.
delegation_creationTime :: Lens.Lens' Delegation (Prelude.Maybe Prelude.UTCTime)
delegation_creationTime = Lens.lens (\Delegation' {creationTime} -> creationTime) (\s@Delegation' {} a -> s {creationTime = a} :: Delegation) Prelude.. Lens.mapping Core._Time

-- | The status of the delegation.
delegation_status :: Lens.Lens' Delegation (Prelude.Maybe DelegationStatus)
delegation_status = Lens.lens (\Delegation' {status} -> status) (\s@Delegation' {} a -> s {status = a} :: Delegation)

-- | Specifies when the delegation was last updated.
delegation_lastUpdated :: Lens.Lens' Delegation (Prelude.Maybe Prelude.UTCTime)
delegation_lastUpdated = Lens.lens (\Delegation' {lastUpdated} -> lastUpdated) (\s@Delegation' {} a -> s {lastUpdated = a} :: Delegation) Prelude.. Lens.mapping Core._Time

-- | The identifier for the associated control set.
delegation_controlSetId :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_controlSetId = Lens.lens (\Delegation' {controlSetId} -> controlSetId) (\s@Delegation' {} a -> s {controlSetId = a} :: Delegation)

-- | The IAM user or role that created the delegation.
delegation_createdBy :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_createdBy = Lens.lens (\Delegation' {createdBy} -> createdBy) (\s@Delegation' {} a -> s {createdBy = a} :: Delegation)

-- | The unique identifier for the delegation.
delegation_id :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_id = Lens.lens (\Delegation' {id} -> id) (\s@Delegation' {} a -> s {id = a} :: Delegation)

-- | The identifier for the associated assessment.
delegation_assessmentId :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_assessmentId = Lens.lens (\Delegation' {assessmentId} -> assessmentId) (\s@Delegation' {} a -> s {assessmentId = a} :: Delegation)

-- | The comment related to the delegation.
delegation_comment :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_comment = Lens.lens (\Delegation' {comment} -> comment) (\s@Delegation' {} a -> s {comment = a} :: Delegation)

-- | The Amazon Resource Name (ARN) of the IAM role.
delegation_roleArn :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_roleArn = Lens.lens (\Delegation' {roleArn} -> roleArn) (\s@Delegation' {} a -> s {roleArn = a} :: Delegation)

-- | The name of the associated assessment.
delegation_assessmentName :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_assessmentName = Lens.lens (\Delegation' {assessmentName} -> assessmentName) (\s@Delegation' {} a -> s {assessmentName = a} :: Delegation)

instance Core.FromJSON Delegation where
  parseJSON =
    Core.withObject
      "Delegation"
      ( \x ->
          Delegation'
            Prelude.<$> (x Core..:? "roleType")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "controlSetId")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "assessmentId")
            Prelude.<*> (x Core..:? "comment")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "assessmentName")
      )

instance Prelude.Hashable Delegation where
  hashWithSalt _salt Delegation' {..} =
    _salt `Prelude.hashWithSalt` roleType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` assessmentName

instance Prelude.NFData Delegation where
  rnf Delegation' {..} =
    Prelude.rnf roleType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf assessmentName
