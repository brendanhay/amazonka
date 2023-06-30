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
-- Module      : Amazonka.Organizations.Types.ResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.ResourcePolicySummary
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains details about a resource policy.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The policy text of the resource policy.
    content :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains resource policy ID and Amazon Resource Name
    -- (ARN).
    resourcePolicySummary :: Prelude.Maybe ResourcePolicySummary
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
-- 'content', 'resourcePolicy_content' - The policy text of the resource policy.
--
-- 'resourcePolicySummary', 'resourcePolicy_resourcePolicySummary' - A structure that contains resource policy ID and Amazon Resource Name
-- (ARN).
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { content = Prelude.Nothing,
      resourcePolicySummary = Prelude.Nothing
    }

-- | The policy text of the resource policy.
resourcePolicy_content :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_content = Lens.lens (\ResourcePolicy' {content} -> content) (\s@ResourcePolicy' {} a -> s {content = a} :: ResourcePolicy)

-- | A structure that contains resource policy ID and Amazon Resource Name
-- (ARN).
resourcePolicy_resourcePolicySummary :: Lens.Lens' ResourcePolicy (Prelude.Maybe ResourcePolicySummary)
resourcePolicy_resourcePolicySummary = Lens.lens (\ResourcePolicy' {resourcePolicySummary} -> resourcePolicySummary) (\s@ResourcePolicy' {} a -> s {resourcePolicySummary = a} :: ResourcePolicy)

instance Data.FromJSON ResourcePolicy where
  parseJSON =
    Data.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "ResourcePolicySummary")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` resourcePolicySummary

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf resourcePolicySummary
