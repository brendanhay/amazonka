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
-- Module      : Amazonka.SecurityHub.Types.AwsIamRolePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamRolePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An inline policy that is embedded in the role.
--
-- /See:/ 'newAwsIamRolePolicy' smart constructor.
data AwsIamRolePolicy = AwsIamRolePolicy'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'awsIamRolePolicy_policyName' - The name of the policy.
newAwsIamRolePolicy ::
  AwsIamRolePolicy
newAwsIamRolePolicy =
  AwsIamRolePolicy' {policyName = Prelude.Nothing}

-- | The name of the policy.
awsIamRolePolicy_policyName :: Lens.Lens' AwsIamRolePolicy (Prelude.Maybe Prelude.Text)
awsIamRolePolicy_policyName = Lens.lens (\AwsIamRolePolicy' {policyName} -> policyName) (\s@AwsIamRolePolicy' {} a -> s {policyName = a} :: AwsIamRolePolicy)

instance Data.FromJSON AwsIamRolePolicy where
  parseJSON =
    Data.withObject
      "AwsIamRolePolicy"
      ( \x ->
          AwsIamRolePolicy'
            Prelude.<$> (x Data..:? "PolicyName")
      )

instance Prelude.Hashable AwsIamRolePolicy where
  hashWithSalt _salt AwsIamRolePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName

instance Prelude.NFData AwsIamRolePolicy where
  rnf AwsIamRolePolicy' {..} = Prelude.rnf policyName

instance Data.ToJSON AwsIamRolePolicy where
  toJSON AwsIamRolePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("PolicyName" Data..=) Prelude.<$> policyName]
      )
