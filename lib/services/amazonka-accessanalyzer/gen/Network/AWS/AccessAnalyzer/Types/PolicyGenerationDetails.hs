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
-- Module      : Network.AWS.AccessAnalyzer.Types.PolicyGenerationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.PolicyGenerationDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the ARN details about the IAM entity for which the policy is
-- generated.
--
-- /See:/ 'newPolicyGenerationDetails' smart constructor.
data PolicyGenerationDetails = PolicyGenerationDetails'
  { -- | The ARN of the IAM entity (user or role) for which you are generating a
    -- policy.
    principalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyGenerationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalArn', 'policyGenerationDetails_principalArn' - The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
newPolicyGenerationDetails ::
  -- | 'principalArn'
  Prelude.Text ->
  PolicyGenerationDetails
newPolicyGenerationDetails pPrincipalArn_ =
  PolicyGenerationDetails'
    { principalArn =
        pPrincipalArn_
    }

-- | The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
policyGenerationDetails_principalArn :: Lens.Lens' PolicyGenerationDetails Prelude.Text
policyGenerationDetails_principalArn = Lens.lens (\PolicyGenerationDetails' {principalArn} -> principalArn) (\s@PolicyGenerationDetails' {} a -> s {principalArn = a} :: PolicyGenerationDetails)

instance Prelude.Hashable PolicyGenerationDetails

instance Prelude.NFData PolicyGenerationDetails

instance Core.ToJSON PolicyGenerationDetails where
  toJSON PolicyGenerationDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("principalArn" Core..= principalArn)]
      )
