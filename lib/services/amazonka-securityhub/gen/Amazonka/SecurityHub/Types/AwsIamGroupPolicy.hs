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
-- Module      : Amazonka.SecurityHub.Types.AwsIamGroupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamGroupPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A managed policy that is attached to the IAM group.
--
-- /See:/ 'newAwsIamGroupPolicy' smart constructor.
data AwsIamGroupPolicy = AwsIamGroupPolicy'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'awsIamGroupPolicy_policyName' - The name of the policy.
newAwsIamGroupPolicy ::
  AwsIamGroupPolicy
newAwsIamGroupPolicy =
  AwsIamGroupPolicy' {policyName = Prelude.Nothing}

-- | The name of the policy.
awsIamGroupPolicy_policyName :: Lens.Lens' AwsIamGroupPolicy (Prelude.Maybe Prelude.Text)
awsIamGroupPolicy_policyName = Lens.lens (\AwsIamGroupPolicy' {policyName} -> policyName) (\s@AwsIamGroupPolicy' {} a -> s {policyName = a} :: AwsIamGroupPolicy)

instance Core.FromJSON AwsIamGroupPolicy where
  parseJSON =
    Core.withObject
      "AwsIamGroupPolicy"
      ( \x ->
          AwsIamGroupPolicy'
            Prelude.<$> (x Core..:? "PolicyName")
      )

instance Prelude.Hashable AwsIamGroupPolicy where
  hashWithSalt _salt AwsIamGroupPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName

instance Prelude.NFData AwsIamGroupPolicy where
  rnf AwsIamGroupPolicy' {..} = Prelude.rnf policyName

instance Core.ToJSON AwsIamGroupPolicy where
  toJSON AwsIamGroupPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [("PolicyName" Core..=) Prelude.<$> policyName]
      )
