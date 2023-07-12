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
-- Module      : Amazonka.RedshiftServerLess.Types.ResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resource policy object. Currently, you can use policies to share
-- snapshots across Amazon Web Services accounts.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The resource policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the policy.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'resourcePolicy_policy' - The resource policy.
--
-- 'resourceArn', 'resourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the policy.
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { policy = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The resource policy.
resourcePolicy_policy :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policy = Lens.lens (\ResourcePolicy' {policy} -> policy) (\s@ResourcePolicy' {} a -> s {policy = a} :: ResourcePolicy)

-- | The Amazon Resource Name (ARN) of the policy.
resourcePolicy_resourceArn :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_resourceArn = Lens.lens (\ResourcePolicy' {resourceArn} -> resourceArn) (\s@ResourcePolicy' {} a -> s {resourceArn = a} :: ResourcePolicy)

instance Data.FromJSON ResourcePolicy where
  parseJSON =
    Data.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Data..:? "policy")
            Prelude.<*> (x Data..:? "resourceArn")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf resourceArn
