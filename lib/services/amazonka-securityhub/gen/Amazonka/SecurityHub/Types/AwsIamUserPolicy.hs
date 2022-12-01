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
-- Module      : Amazonka.SecurityHub.Types.AwsIamUserPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamUserPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an inline policy that is embedded in the user.
--
-- /See:/ 'newAwsIamUserPolicy' smart constructor.
data AwsIamUserPolicy = AwsIamUserPolicy'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamUserPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'awsIamUserPolicy_policyName' - The name of the policy.
newAwsIamUserPolicy ::
  AwsIamUserPolicy
newAwsIamUserPolicy =
  AwsIamUserPolicy' {policyName = Prelude.Nothing}

-- | The name of the policy.
awsIamUserPolicy_policyName :: Lens.Lens' AwsIamUserPolicy (Prelude.Maybe Prelude.Text)
awsIamUserPolicy_policyName = Lens.lens (\AwsIamUserPolicy' {policyName} -> policyName) (\s@AwsIamUserPolicy' {} a -> s {policyName = a} :: AwsIamUserPolicy)

instance Core.FromJSON AwsIamUserPolicy where
  parseJSON =
    Core.withObject
      "AwsIamUserPolicy"
      ( \x ->
          AwsIamUserPolicy'
            Prelude.<$> (x Core..:? "PolicyName")
      )

instance Prelude.Hashable AwsIamUserPolicy where
  hashWithSalt _salt AwsIamUserPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName

instance Prelude.NFData AwsIamUserPolicy where
  rnf AwsIamUserPolicy' {..} = Prelude.rnf policyName

instance Core.ToJSON AwsIamUserPolicy where
  toJSON AwsIamUserPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [("PolicyName" Core..=) Prelude.<$> policyName]
      )
