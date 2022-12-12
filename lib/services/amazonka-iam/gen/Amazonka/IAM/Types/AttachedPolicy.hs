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
-- Module      : Amazonka.IAM.Types.AttachedPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AttachedPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an attached policy.
--
-- An attached policy is a managed policy that has been attached to a user,
-- group, or role. This data type is used as a response element in the
-- ListAttachedGroupPolicies, ListAttachedRolePolicies,
-- ListAttachedUserPolicies, and GetAccountAuthorizationDetails operations.
--
-- For more information about managed policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newAttachedPolicy' smart constructor.
data AttachedPolicy = AttachedPolicy'
  { policyArn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the attached policy.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachedPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'attachedPolicy_policyArn' - Undocumented member.
--
-- 'policyName', 'attachedPolicy_policyName' - The friendly name of the attached policy.
newAttachedPolicy ::
  AttachedPolicy
newAttachedPolicy =
  AttachedPolicy'
    { policyArn = Prelude.Nothing,
      policyName = Prelude.Nothing
    }

-- | Undocumented member.
attachedPolicy_policyArn :: Lens.Lens' AttachedPolicy (Prelude.Maybe Prelude.Text)
attachedPolicy_policyArn = Lens.lens (\AttachedPolicy' {policyArn} -> policyArn) (\s@AttachedPolicy' {} a -> s {policyArn = a} :: AttachedPolicy)

-- | The friendly name of the attached policy.
attachedPolicy_policyName :: Lens.Lens' AttachedPolicy (Prelude.Maybe Prelude.Text)
attachedPolicy_policyName = Lens.lens (\AttachedPolicy' {policyName} -> policyName) (\s@AttachedPolicy' {} a -> s {policyName = a} :: AttachedPolicy)

instance Data.FromXML AttachedPolicy where
  parseXML x =
    AttachedPolicy'
      Prelude.<$> (x Data..@? "PolicyArn")
      Prelude.<*> (x Data..@? "PolicyName")

instance Prelude.Hashable AttachedPolicy where
  hashWithSalt _salt AttachedPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData AttachedPolicy where
  rnf AttachedPolicy' {..} =
    Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf policyName
