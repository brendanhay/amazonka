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
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identities.
    identities :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The Amazon Resource Name (ARN) for the IAM policy.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | Assignment name.
    assignmentName :: Prelude.Maybe Prelude.Text,
    -- | Assignment ID.
    assignmentId :: Prelude.Maybe Prelude.Text,
    -- | Assignment status.
    assignmentStatus :: Prelude.Maybe AssignmentStatus
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
-- 'awsAccountId', 'iAMPolicyAssignment_awsAccountId' - The Amazon Web Services account ID.
--
-- 'identities', 'iAMPolicyAssignment_identities' - Identities.
--
-- 'policyArn', 'iAMPolicyAssignment_policyArn' - The Amazon Resource Name (ARN) for the IAM policy.
--
-- 'assignmentName', 'iAMPolicyAssignment_assignmentName' - Assignment name.
--
-- 'assignmentId', 'iAMPolicyAssignment_assignmentId' - Assignment ID.
--
-- 'assignmentStatus', 'iAMPolicyAssignment_assignmentStatus' - Assignment status.
newIAMPolicyAssignment ::
  IAMPolicyAssignment
newIAMPolicyAssignment =
  IAMPolicyAssignment'
    { awsAccountId =
        Prelude.Nothing,
      identities = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      assignmentName = Prelude.Nothing,
      assignmentId = Prelude.Nothing,
      assignmentStatus = Prelude.Nothing
    }

-- | The Amazon Web Services account ID.
iAMPolicyAssignment_awsAccountId :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_awsAccountId = Lens.lens (\IAMPolicyAssignment' {awsAccountId} -> awsAccountId) (\s@IAMPolicyAssignment' {} a -> s {awsAccountId = a} :: IAMPolicyAssignment)

-- | Identities.
iAMPolicyAssignment_identities :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
iAMPolicyAssignment_identities = Lens.lens (\IAMPolicyAssignment' {identities} -> identities) (\s@IAMPolicyAssignment' {} a -> s {identities = a} :: IAMPolicyAssignment) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the IAM policy.
iAMPolicyAssignment_policyArn :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_policyArn = Lens.lens (\IAMPolicyAssignment' {policyArn} -> policyArn) (\s@IAMPolicyAssignment' {} a -> s {policyArn = a} :: IAMPolicyAssignment)

-- | Assignment name.
iAMPolicyAssignment_assignmentName :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_assignmentName = Lens.lens (\IAMPolicyAssignment' {assignmentName} -> assignmentName) (\s@IAMPolicyAssignment' {} a -> s {assignmentName = a} :: IAMPolicyAssignment)

-- | Assignment ID.
iAMPolicyAssignment_assignmentId :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe Prelude.Text)
iAMPolicyAssignment_assignmentId = Lens.lens (\IAMPolicyAssignment' {assignmentId} -> assignmentId) (\s@IAMPolicyAssignment' {} a -> s {assignmentId = a} :: IAMPolicyAssignment)

-- | Assignment status.
iAMPolicyAssignment_assignmentStatus :: Lens.Lens' IAMPolicyAssignment (Prelude.Maybe AssignmentStatus)
iAMPolicyAssignment_assignmentStatus = Lens.lens (\IAMPolicyAssignment' {assignmentStatus} -> assignmentStatus) (\s@IAMPolicyAssignment' {} a -> s {assignmentStatus = a} :: IAMPolicyAssignment)

instance Data.FromJSON IAMPolicyAssignment where
  parseJSON =
    Data.withObject
      "IAMPolicyAssignment"
      ( \x ->
          IAMPolicyAssignment'
            Prelude.<$> (x Data..:? "AwsAccountId")
            Prelude.<*> (x Data..:? "Identities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PolicyArn")
            Prelude.<*> (x Data..:? "AssignmentName")
            Prelude.<*> (x Data..:? "AssignmentId")
            Prelude.<*> (x Data..:? "AssignmentStatus")
      )

instance Prelude.Hashable IAMPolicyAssignment where
  hashWithSalt _salt IAMPolicyAssignment' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` identities
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` assignmentName
      `Prelude.hashWithSalt` assignmentId
      `Prelude.hashWithSalt` assignmentStatus

instance Prelude.NFData IAMPolicyAssignment where
  rnf IAMPolicyAssignment' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf identities
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf assignmentName
      `Prelude.seq` Prelude.rnf assignmentId
      `Prelude.seq` Prelude.rnf assignmentStatus
