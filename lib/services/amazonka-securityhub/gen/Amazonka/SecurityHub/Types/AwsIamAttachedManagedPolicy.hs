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
-- Module      : Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A managed policy that is attached to an IAM principal.
--
-- /See:/ 'newAwsIamAttachedManagedPolicy' smart constructor.
data AwsIamAttachedManagedPolicy = AwsIamAttachedManagedPolicy'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy.
    policyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamAttachedManagedPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'awsIamAttachedManagedPolicy_policyName' - The name of the policy.
--
-- 'policyArn', 'awsIamAttachedManagedPolicy_policyArn' - The ARN of the policy.
newAwsIamAttachedManagedPolicy ::
  AwsIamAttachedManagedPolicy
newAwsIamAttachedManagedPolicy =
  AwsIamAttachedManagedPolicy'
    { policyName =
        Prelude.Nothing,
      policyArn = Prelude.Nothing
    }

-- | The name of the policy.
awsIamAttachedManagedPolicy_policyName :: Lens.Lens' AwsIamAttachedManagedPolicy (Prelude.Maybe Prelude.Text)
awsIamAttachedManagedPolicy_policyName = Lens.lens (\AwsIamAttachedManagedPolicy' {policyName} -> policyName) (\s@AwsIamAttachedManagedPolicy' {} a -> s {policyName = a} :: AwsIamAttachedManagedPolicy)

-- | The ARN of the policy.
awsIamAttachedManagedPolicy_policyArn :: Lens.Lens' AwsIamAttachedManagedPolicy (Prelude.Maybe Prelude.Text)
awsIamAttachedManagedPolicy_policyArn = Lens.lens (\AwsIamAttachedManagedPolicy' {policyArn} -> policyArn) (\s@AwsIamAttachedManagedPolicy' {} a -> s {policyArn = a} :: AwsIamAttachedManagedPolicy)

instance Core.FromJSON AwsIamAttachedManagedPolicy where
  parseJSON =
    Core.withObject
      "AwsIamAttachedManagedPolicy"
      ( \x ->
          AwsIamAttachedManagedPolicy'
            Prelude.<$> (x Core..:? "PolicyName")
            Prelude.<*> (x Core..:? "PolicyArn")
      )

instance Prelude.Hashable AwsIamAttachedManagedPolicy where
  hashWithSalt _salt AwsIamAttachedManagedPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData AwsIamAttachedManagedPolicy where
  rnf AwsIamAttachedManagedPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyArn

instance Core.ToJSON AwsIamAttachedManagedPolicy where
  toJSON AwsIamAttachedManagedPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PolicyName" Core..=) Prelude.<$> policyName,
            ("PolicyArn" Core..=) Prelude.<$> policyArn
          ]
      )
