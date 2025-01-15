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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Delegation where

import Amazonka.AuditManager.Types.DelegationStatus
import Amazonka.AuditManager.Types.RoleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The assignment of a control set to a delegate for review.
--
-- /See:/ 'newDelegation' smart constructor.
data Delegation = Delegation'
  { -- | The identifier for the assessment that\'s associated with the
    -- delegation.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the assessment that\'s associated with the delegation.
    assessmentName :: Prelude.Maybe Prelude.Text,
    -- | The comment that\'s related to the delegation.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the control set that\'s associated with the
    -- delegation.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The IAM user or role that created the delegation.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the delegation was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the delegation.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the delegation was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The type of customer persona.
    --
    -- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
    -- @RESOURCE_OWNER@.
    roleType :: Prelude.Maybe RoleType,
    -- | The status of the delegation.
    status :: Prelude.Maybe DelegationStatus
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
-- 'assessmentId', 'delegation_assessmentId' - The identifier for the assessment that\'s associated with the
-- delegation.
--
-- 'assessmentName', 'delegation_assessmentName' - The name of the assessment that\'s associated with the delegation.
--
-- 'comment', 'delegation_comment' - The comment that\'s related to the delegation.
--
-- 'controlSetId', 'delegation_controlSetId' - The identifier for the control set that\'s associated with the
-- delegation.
--
-- 'createdBy', 'delegation_createdBy' - The IAM user or role that created the delegation.
--
-- 'creationTime', 'delegation_creationTime' - Specifies when the delegation was created.
--
-- 'id', 'delegation_id' - The unique identifier for the delegation.
--
-- 'lastUpdated', 'delegation_lastUpdated' - Specifies when the delegation was last updated.
--
-- 'roleArn', 'delegation_roleArn' - The Amazon Resource Name (ARN) of the IAM role.
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
-- 'status', 'delegation_status' - The status of the delegation.
newDelegation ::
  Delegation
newDelegation =
  Delegation'
    { assessmentId = Prelude.Nothing,
      assessmentName = Prelude.Nothing,
      comment = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      roleType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The identifier for the assessment that\'s associated with the
-- delegation.
delegation_assessmentId :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_assessmentId = Lens.lens (\Delegation' {assessmentId} -> assessmentId) (\s@Delegation' {} a -> s {assessmentId = a} :: Delegation)

-- | The name of the assessment that\'s associated with the delegation.
delegation_assessmentName :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_assessmentName = Lens.lens (\Delegation' {assessmentName} -> assessmentName) (\s@Delegation' {} a -> s {assessmentName = a} :: Delegation)

-- | The comment that\'s related to the delegation.
delegation_comment :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_comment = Lens.lens (\Delegation' {comment} -> comment) (\s@Delegation' {} a -> s {comment = a} :: Delegation)

-- | The identifier for the control set that\'s associated with the
-- delegation.
delegation_controlSetId :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_controlSetId = Lens.lens (\Delegation' {controlSetId} -> controlSetId) (\s@Delegation' {} a -> s {controlSetId = a} :: Delegation)

-- | The IAM user or role that created the delegation.
delegation_createdBy :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_createdBy = Lens.lens (\Delegation' {createdBy} -> createdBy) (\s@Delegation' {} a -> s {createdBy = a} :: Delegation)

-- | Specifies when the delegation was created.
delegation_creationTime :: Lens.Lens' Delegation (Prelude.Maybe Prelude.UTCTime)
delegation_creationTime = Lens.lens (\Delegation' {creationTime} -> creationTime) (\s@Delegation' {} a -> s {creationTime = a} :: Delegation) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the delegation.
delegation_id :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_id = Lens.lens (\Delegation' {id} -> id) (\s@Delegation' {} a -> s {id = a} :: Delegation)

-- | Specifies when the delegation was last updated.
delegation_lastUpdated :: Lens.Lens' Delegation (Prelude.Maybe Prelude.UTCTime)
delegation_lastUpdated = Lens.lens (\Delegation' {lastUpdated} -> lastUpdated) (\s@Delegation' {} a -> s {lastUpdated = a} :: Delegation) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the IAM role.
delegation_roleArn :: Lens.Lens' Delegation (Prelude.Maybe Prelude.Text)
delegation_roleArn = Lens.lens (\Delegation' {roleArn} -> roleArn) (\s@Delegation' {} a -> s {roleArn = a} :: Delegation)

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

-- | The status of the delegation.
delegation_status :: Lens.Lens' Delegation (Prelude.Maybe DelegationStatus)
delegation_status = Lens.lens (\Delegation' {status} -> status) (\s@Delegation' {} a -> s {status = a} :: Delegation)

instance Data.FromJSON Delegation where
  parseJSON =
    Data.withObject
      "Delegation"
      ( \x ->
          Delegation'
            Prelude.<$> (x Data..:? "assessmentId")
            Prelude.<*> (x Data..:? "assessmentName")
            Prelude.<*> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "controlSetId")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "roleType")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Delegation where
  hashWithSalt _salt Delegation' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` roleType
      `Prelude.hashWithSalt` status

instance Prelude.NFData Delegation where
  rnf Delegation' {..} =
    Prelude.rnf assessmentId `Prelude.seq`
      Prelude.rnf assessmentName `Prelude.seq`
        Prelude.rnf comment `Prelude.seq`
          Prelude.rnf controlSetId `Prelude.seq`
            Prelude.rnf createdBy `Prelude.seq`
              Prelude.rnf creationTime `Prelude.seq`
                Prelude.rnf id `Prelude.seq`
                  Prelude.rnf lastUpdated `Prelude.seq`
                    Prelude.rnf roleArn `Prelude.seq`
                      Prelude.rnf roleType `Prelude.seq`
                        Prelude.rnf status
