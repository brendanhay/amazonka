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
-- Module      : Amazonka.AccessAnalyzer.Types.GeneratedPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.GeneratedPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the text for the generated policy.
--
-- /See:/ 'newGeneratedPolicy' smart constructor.
data GeneratedPolicy = GeneratedPolicy'
  { -- | The text to use as the content for the new policy. The policy is created
    -- using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html CreatePolicy>
    -- action.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneratedPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'generatedPolicy_policy' - The text to use as the content for the new policy. The policy is created
-- using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html CreatePolicy>
-- action.
newGeneratedPolicy ::
  -- | 'policy'
  Prelude.Text ->
  GeneratedPolicy
newGeneratedPolicy pPolicy_ =
  GeneratedPolicy' {policy = pPolicy_}

-- | The text to use as the content for the new policy. The policy is created
-- using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html CreatePolicy>
-- action.
generatedPolicy_policy :: Lens.Lens' GeneratedPolicy Prelude.Text
generatedPolicy_policy = Lens.lens (\GeneratedPolicy' {policy} -> policy) (\s@GeneratedPolicy' {} a -> s {policy = a} :: GeneratedPolicy)

instance Core.FromJSON GeneratedPolicy where
  parseJSON =
    Core.withObject
      "GeneratedPolicy"
      ( \x ->
          GeneratedPolicy' Prelude.<$> (x Core..: "policy")
      )

instance Prelude.Hashable GeneratedPolicy where
  hashWithSalt _salt GeneratedPolicy' {..} =
    _salt `Prelude.hashWithSalt` policy

instance Prelude.NFData GeneratedPolicy where
  rnf GeneratedPolicy' {..} = Prelude.rnf policy
