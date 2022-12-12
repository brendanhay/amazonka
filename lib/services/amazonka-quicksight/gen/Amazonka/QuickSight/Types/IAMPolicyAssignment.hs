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
-- Module      : Amazonka.QuickSight.Types.IAMPolicyAssignment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IAMPolicyAssignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssignmentStatus

-- | An Identity and Access Management (IAM) policy assignment.
--
-- /See:/ 'newIAMPolicyAssignment' smart constructor.
data IAMPolicyAssignment = IAMPolicyAssignment'
  { -- | Assignment ID.
    assignmentId :: Prelude.Maybe Prelude.Text,
    -- | Assignment name.
    assignmentName :: Prelude.Maybe Prelude.Text,
    -- | Assignment status.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identities.
    identities :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The Amazon Resource Name (ARN) for the IAM policy.
    policyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IAMPolicyAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentId', 'iAMPolicyAssignment_assignmentId' - Assignment ID.
--
-- 'assignmentName', 'iAMPolicyAssignment_assignmentName' - Assignment name.
--
-- 'assignmentStatus', 'iAMPolicyAssignment_assignmentStatus' - Assignment status.
--
-- 'awsAccountId', 'iAMPolicyAssignment_awsAccountId' - The Amazon Web Services account ID.
--
-- 'identities', 'iAMPolicyAssignment_identities' - Identities.
--
-- 'policyArn', 'iAMPolicyAssignment_policyArn' - The Amazon Resource Name (ARN) for the IAM policy.
newIAMPolicyAssignment ::
  IAMPolicyAssignment
newIAMPolicyAssignment =
  IAMPolicyAssignment'
    { assignmentId =
        Prelude.Nothing,
      assignmentName = Prelude.Nothing,
      assignmentStatus = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      identities = Prelude.Nothing,
      policyArn = Prelude.Nothing
    }

-- | Assignment ID.
iAMPolicyAssignment_assignmentId :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_assignmentId = Lens.lens (\IAMPolicyAssignment' {assignmentId} -> assignmentId) (\s@IAMPolicyAssignment' {} a -> s {assignmentId = a} :: IAMPolicyAssignment)

-- | Assignment name.
iAMPolicyAssignment_assignmentName :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_assignmentName = Lens.lens (\IAMPolicyAssignment' {assignmentName} -> assignmentName) (\s@IAMPolicyAssignment' {} a -> s {assignmentName = a} :: IAMPolicyAssignment)

-- | Assignment status.
iAMPolicyAssignment_assignmentStatus :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe AssignmentStatus)
iAMPolicyAssignment_assignmentStatus = Lens.lens (\IAMPolicyAssignment' {assignmentStatus} -> assignmentStatus) (\s@IAMPolicyAssignment' {} a -> s {assignmentStatus = a} :: IAMPolicyAssignment)

-- | The Amazon Web Services account ID.
iAMPolicyAssignment_awsAccountId :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_awsAccountId = Lens.lens (\IAMPolicyAssignment' {awsAccountId} -> awsAccountId) (\s@IAMPolicyAssignment' {} a -> s {awsAccountId = a} :: IAMPolicyAssignment)

-- | Identities.
iAMPolicyAssignment_identities :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
iAMPolicyAssignment_identities = Lens.lens (\IAMPolicyAssignment' {identities} -> identities) (\s@IAMPolicyAssignment' {} a -> s {identities = a} :: IAMPolicyAssignment) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the IAM policy.
iAMPolicyAssignment_policyArn :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_policyArn = Lens.lens (\IAMPolicyAssignment' {policyArn} -> policyArn) (\s@IAMPolicyAssignment' {} a -> s {policyArn = a} :: IAMPolicyAssignment)

instance Data.FromJSON IAMPolicyAssignment where
  parseJSON =
    Data.withObject
      "IAMPolicyAssignment"
      ( \x ->
          IAMPolicyAssignment'
            Prelude.<$> (x Data..:? "AssignmentId")
            Prelude.<*> (x Data..:? "AssignmentName")
            Prelude.<*> (x Data..:? "AssignmentStatus")
            Prelude.<*> (x Data..:? "AwsAccountId")
            Prelude.<*> (x Data..:? "Identities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PolicyArn")
      )

instance Prelude.Hashable IAMPolicyAssignment where
  hashWithSalt _salt IAMPolicyAssignment' {..} =
    _salt `Prelude.hashWithSalt` assignmentId
      `Prelude.hashWithSalt` assignmentName
      `Prelude.hashWithSalt` assignmentStatus
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` identities
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData IAMPolicyAssignment where
  rnf IAMPolicyAssignment' {..} =
    Prelude.rnf assignmentId
      `Prelude.seq` Prelude.rnf assignmentName
      `Prelude.seq` Prelude.rnf assignmentStatus
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf identities
      `Prelude.seq` Prelude.rnf policyArn
