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
-- Module      : Network.AWS.QuickSight.Types.IAMPolicyAssignmentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.IAMPolicyAssignmentSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.AssignmentStatus

-- | IAMpolicy assignment summary.
--
-- /See:/ 'newIAMPolicyAssignmentSummary' smart constructor.
data IAMPolicyAssignmentSummary = IAMPolicyAssignmentSummary'
  { -- | Assignment name.
    assignmentName :: Prelude.Maybe Prelude.Text,
    -- | Assignment status.
    assignmentStatus :: Prelude.Maybe AssignmentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IAMPolicyAssignmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentName', 'iAMPolicyAssignmentSummary_assignmentName' - Assignment name.
--
-- 'assignmentStatus', 'iAMPolicyAssignmentSummary_assignmentStatus' - Assignment status.
newIAMPolicyAssignmentSummary ::
  IAMPolicyAssignmentSummary
newIAMPolicyAssignmentSummary =
  IAMPolicyAssignmentSummary'
    { assignmentName =
        Prelude.Nothing,
      assignmentStatus = Prelude.Nothing
    }

-- | Assignment name.
iAMPolicyAssignmentSummary_assignmentName :: Lens.Lens' IAMPolicyAssignmentSummary (Prelude.Maybe Prelude.Text)
iAMPolicyAssignmentSummary_assignmentName = Lens.lens (\IAMPolicyAssignmentSummary' {assignmentName} -> assignmentName) (\s@IAMPolicyAssignmentSummary' {} a -> s {assignmentName = a} :: IAMPolicyAssignmentSummary)

-- | Assignment status.
iAMPolicyAssignmentSummary_assignmentStatus :: Lens.Lens' IAMPolicyAssignmentSummary (Prelude.Maybe AssignmentStatus)
iAMPolicyAssignmentSummary_assignmentStatus = Lens.lens (\IAMPolicyAssignmentSummary' {assignmentStatus} -> assignmentStatus) (\s@IAMPolicyAssignmentSummary' {} a -> s {assignmentStatus = a} :: IAMPolicyAssignmentSummary)

instance Core.FromJSON IAMPolicyAssignmentSummary where
  parseJSON =
    Core.withObject
      "IAMPolicyAssignmentSummary"
      ( \x ->
          IAMPolicyAssignmentSummary'
            Prelude.<$> (x Core..:? "AssignmentName")
            Prelude.<*> (x Core..:? "AssignmentStatus")
      )

instance Prelude.Hashable IAMPolicyAssignmentSummary

instance Prelude.NFData IAMPolicyAssignmentSummary
