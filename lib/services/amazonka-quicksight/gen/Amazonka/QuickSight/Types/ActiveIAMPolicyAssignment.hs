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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | A name for the IAM policy assignment.
    assignmentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource.
    policyArn :: Prelude.Maybe Prelude.Text
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
-- 'assignmentName', 'activeIAMPolicyAssignment_assignmentName' - A name for the IAM policy assignment.
--
-- 'policyArn', 'activeIAMPolicyAssignment_policyArn' - The Amazon Resource Name (ARN) of the resource.
newActiveIAMPolicyAssignment ::
  ActiveIAMPolicyAssignment
newActiveIAMPolicyAssignment =
  ActiveIAMPolicyAssignment'
    { assignmentName =
        Prelude.Nothing,
      policyArn = Prelude.Nothing
    }

-- | A name for the IAM policy assignment.
activeIAMPolicyAssignment_assignmentName :: Lens.Lens' ActiveIAMPolicyAssignment (Prelude.Maybe Prelude.Text)
activeIAMPolicyAssignment_assignmentName = Lens.lens (\ActiveIAMPolicyAssignment' {assignmentName} -> assignmentName) (\s@ActiveIAMPolicyAssignment' {} a -> s {assignmentName = a} :: ActiveIAMPolicyAssignment)

-- | The Amazon Resource Name (ARN) of the resource.
activeIAMPolicyAssignment_policyArn :: Lens.Lens' ActiveIAMPolicyAssignment (Prelude.Maybe Prelude.Text)
activeIAMPolicyAssignment_policyArn = Lens.lens (\ActiveIAMPolicyAssignment' {policyArn} -> policyArn) (\s@ActiveIAMPolicyAssignment' {} a -> s {policyArn = a} :: ActiveIAMPolicyAssignment)

instance Data.FromJSON ActiveIAMPolicyAssignment where
  parseJSON =
    Data.withObject
      "ActiveIAMPolicyAssignment"
      ( \x ->
          ActiveIAMPolicyAssignment'
            Prelude.<$> (x Data..:? "AssignmentName")
            Prelude.<*> (x Data..:? "PolicyArn")
      )

instance Prelude.Hashable ActiveIAMPolicyAssignment where
  hashWithSalt _salt ActiveIAMPolicyAssignment' {..} =
    _salt
      `Prelude.hashWithSalt` assignmentName
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData ActiveIAMPolicyAssignment where
  rnf ActiveIAMPolicyAssignment' {..} =
    Prelude.rnf assignmentName `Prelude.seq`
      Prelude.rnf policyArn
