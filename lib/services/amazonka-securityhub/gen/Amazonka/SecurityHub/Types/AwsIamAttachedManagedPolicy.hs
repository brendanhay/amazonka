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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A managed policy that is attached to an IAM principal.
--
-- /See:/ 'newAwsIamAttachedManagedPolicy' smart constructor.
data AwsIamAttachedManagedPolicy = AwsIamAttachedManagedPolicy'
  { -- | The ARN of the policy.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text
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
-- 'policyArn', 'awsIamAttachedManagedPolicy_policyArn' - The ARN of the policy.
--
-- 'policyName', 'awsIamAttachedManagedPolicy_policyName' - The name of the policy.
newAwsIamAttachedManagedPolicy ::
  AwsIamAttachedManagedPolicy
newAwsIamAttachedManagedPolicy =
  AwsIamAttachedManagedPolicy'
    { policyArn =
        Prelude.Nothing,
      policyName = Prelude.Nothing
    }

-- | The ARN of the policy.
awsIamAttachedManagedPolicy_policyArn :: Lens.Lens' AwsIamAttachedManagedPolicy (Prelude.Maybe Prelude.Text)
awsIamAttachedManagedPolicy_policyArn = Lens.lens (\AwsIamAttachedManagedPolicy' {policyArn} -> policyArn) (\s@AwsIamAttachedManagedPolicy' {} a -> s {policyArn = a} :: AwsIamAttachedManagedPolicy)

-- | The name of the policy.
awsIamAttachedManagedPolicy_policyName :: Lens.Lens' AwsIamAttachedManagedPolicy (Prelude.Maybe Prelude.Text)
awsIamAttachedManagedPolicy_policyName = Lens.lens (\AwsIamAttachedManagedPolicy' {policyName} -> policyName) (\s@AwsIamAttachedManagedPolicy' {} a -> s {policyName = a} :: AwsIamAttachedManagedPolicy)

instance Data.FromJSON AwsIamAttachedManagedPolicy where
  parseJSON =
    Data.withObject
      "AwsIamAttachedManagedPolicy"
      ( \x ->
          AwsIamAttachedManagedPolicy'
            Prelude.<$> (x Data..:? "PolicyArn")
            Prelude.<*> (x Data..:? "PolicyName")
      )

instance Prelude.Hashable AwsIamAttachedManagedPolicy where
  hashWithSalt _salt AwsIamAttachedManagedPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData AwsIamAttachedManagedPolicy where
  rnf AwsIamAttachedManagedPolicy' {..} =
    Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToJSON AwsIamAttachedManagedPolicy where
  toJSON AwsIamAttachedManagedPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyArn" Data..=) Prelude.<$> policyArn,
            ("PolicyName" Data..=) Prelude.<$> policyName
          ]
      )
