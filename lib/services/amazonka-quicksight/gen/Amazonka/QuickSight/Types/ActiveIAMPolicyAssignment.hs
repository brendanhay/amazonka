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
-- Module      : Amazonka.QuickSight.Types.ActiveIAMPolicyAssignment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ActiveIAMPolicyAssignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The active Identity and Access Management (IAM) policy assignment.
--
-- /See:/ 'newActiveIAMPolicyAssignment' smart constructor.
data ActiveIAMPolicyAssignment = ActiveIAMPolicyAssignment'
  { -- | The Amazon Resource Name (ARN) of the resource.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | A name for the IAM policy assignment.
    assignmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveIAMPolicyAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'activeIAMPolicyAssignment_policyArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'assignmentName', 'activeIAMPolicyAssignment_assignmentName' - A name for the IAM policy assignment.
newActiveIAMPolicyAssignment ::
  ActiveIAMPolicyAssignment
newActiveIAMPolicyAssignment =
  ActiveIAMPolicyAssignment'
    { policyArn =
        Prelude.Nothing,
      assignmentName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
activeIAMPolicyAssignment_policyArn :: Lens.Lens' ActiveIAMPolicyAssignment (Prelude.Maybe Prelude.Text)
activeIAMPolicyAssignment_policyArn = Lens.lens (\ActiveIAMPolicyAssignment' {policyArn} -> policyArn) (\s@ActiveIAMPolicyAssignment' {} a -> s {policyArn = a} :: ActiveIAMPolicyAssignment)

-- | A name for the IAM policy assignment.
activeIAMPolicyAssignment_assignmentName :: Lens.Lens' ActiveIAMPolicyAssignment (Prelude.Maybe Prelude.Text)
activeIAMPolicyAssignment_assignmentName = Lens.lens (\ActiveIAMPolicyAssignment' {assignmentName} -> assignmentName) (\s@ActiveIAMPolicyAssignment' {} a -> s {assignmentName = a} :: ActiveIAMPolicyAssignment)

instance Data.FromJSON ActiveIAMPolicyAssignment where
  parseJSON =
    Data.withObject
      "ActiveIAMPolicyAssignment"
      ( \x ->
          ActiveIAMPolicyAssignment'
            Prelude.<$> (x Data..:? "PolicyArn")
            Prelude.<*> (x Data..:? "AssignmentName")
      )

instance Prelude.Hashable ActiveIAMPolicyAssignment where
  hashWithSalt _salt ActiveIAMPolicyAssignment' {..} =
    _salt `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` assignmentName

instance Prelude.NFData ActiveIAMPolicyAssignment where
  rnf ActiveIAMPolicyAssignment' {..} =
    Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf assignmentName
