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
-- Module      : Network.AWS.AuditManager.Types.DelegationMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.DelegationMetadata where

import Network.AWS.AuditManager.Types.DelegationStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata associated with the specified delegation.
--
-- /See:/ 'newDelegationMetadata' smart constructor.
data DelegationMetadata = DelegationMetadata'
  { -- | Specifies when the delegation was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The current status of the delgation.
    status :: Prelude.Maybe DelegationStatus,
    -- | Specifies the name of the control set delegated for review.
    controlSetName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the delegation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the specified assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated assessment.
    assessmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelegationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'delegationMetadata_creationTime' - Specifies when the delegation was created.
--
-- 'status', 'delegationMetadata_status' - The current status of the delgation.
--
-- 'controlSetName', 'delegationMetadata_controlSetName' - Specifies the name of the control set delegated for review.
--
-- 'id', 'delegationMetadata_id' - The unique identifier for the delegation.
--
-- 'assessmentId', 'delegationMetadata_assessmentId' - The unique identifier for the specified assessment.
--
-- 'roleArn', 'delegationMetadata_roleArn' - The Amazon Resource Name (ARN) of the IAM role.
--
-- 'assessmentName', 'delegationMetadata_assessmentName' - The name of the associated assessment.
newDelegationMetadata ::
  DelegationMetadata
newDelegationMetadata =
  DelegationMetadata'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      controlSetName = Prelude.Nothing,
      id = Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      assessmentName = Prelude.Nothing
    }

-- | Specifies when the delegation was created.
delegationMetadata_creationTime :: Lens.Lens' DelegationMetadata (Prelude.Maybe Prelude.UTCTime)
delegationMetadata_creationTime = Lens.lens (\DelegationMetadata' {creationTime} -> creationTime) (\s@DelegationMetadata' {} a -> s {creationTime = a} :: DelegationMetadata) Prelude.. Lens.mapping Core._Time

-- | The current status of the delgation.
delegationMetadata_status :: Lens.Lens' DelegationMetadata (Prelude.Maybe DelegationStatus)
delegationMetadata_status = Lens.lens (\DelegationMetadata' {status} -> status) (\s@DelegationMetadata' {} a -> s {status = a} :: DelegationMetadata)

-- | Specifies the name of the control set delegated for review.
delegationMetadata_controlSetName :: Lens.Lens' DelegationMetadata (Prelude.Maybe Prelude.Text)
delegationMetadata_controlSetName = Lens.lens (\DelegationMetadata' {controlSetName} -> controlSetName) (\s@DelegationMetadata' {} a -> s {controlSetName = a} :: DelegationMetadata)

-- | The unique identifier for the delegation.
delegationMetadata_id :: Lens.Lens' DelegationMetadata (Prelude.Maybe Prelude.Text)
delegationMetadata_id = Lens.lens (\DelegationMetadata' {id} -> id) (\s@DelegationMetadata' {} a -> s {id = a} :: DelegationMetadata)

-- | The unique identifier for the specified assessment.
delegationMetadata_assessmentId :: Lens.Lens' DelegationMetadata (Prelude.Maybe Prelude.Text)
delegationMetadata_assessmentId = Lens.lens (\DelegationMetadata' {assessmentId} -> assessmentId) (\s@DelegationMetadata' {} a -> s {assessmentId = a} :: DelegationMetadata)

-- | The Amazon Resource Name (ARN) of the IAM role.
delegationMetadata_roleArn :: Lens.Lens' DelegationMetadata (Prelude.Maybe Prelude.Text)
delegationMetadata_roleArn = Lens.lens (\DelegationMetadata' {roleArn} -> roleArn) (\s@DelegationMetadata' {} a -> s {roleArn = a} :: DelegationMetadata)

-- | The name of the associated assessment.
delegationMetadata_assessmentName :: Lens.Lens' DelegationMetadata (Prelude.Maybe Prelude.Text)
delegationMetadata_assessmentName = Lens.lens (\DelegationMetadata' {assessmentName} -> assessmentName) (\s@DelegationMetadata' {} a -> s {assessmentName = a} :: DelegationMetadata)

instance Core.FromJSON DelegationMetadata where
  parseJSON =
    Core.withObject
      "DelegationMetadata"
      ( \x ->
          DelegationMetadata'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "controlSetName")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "assessmentId")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "assessmentName")
      )

instance Prelude.Hashable DelegationMetadata

instance Prelude.NFData DelegationMetadata
