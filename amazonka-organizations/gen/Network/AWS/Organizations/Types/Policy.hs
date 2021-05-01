{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Organizations.Types.Policy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Policy where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicySummary
import qualified Network.AWS.Prelude as Prelude

-- | Contains rules to be applied to the affected accounts. Policies can be
-- attached directly to accounts, or to roots and OUs to affect all
-- accounts in those hierarchies.
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | A structure that contains additional details about the policy.
    policySummary :: Prelude.Maybe PolicySummary,
    -- | The text content of the policy.
    content :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Policy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policySummary', 'policy_policySummary' - A structure that contains additional details about the policy.
--
-- 'content', 'policy_content' - The text content of the policy.
newPolicy ::
  Policy
newPolicy =
  Policy'
    { policySummary = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | A structure that contains additional details about the policy.
policy_policySummary :: Lens.Lens' Policy (Prelude.Maybe PolicySummary)
policy_policySummary = Lens.lens (\Policy' {policySummary} -> policySummary) (\s@Policy' {} a -> s {policySummary = a} :: Policy)

-- | The text content of the policy.
policy_content :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_content = Lens.lens (\Policy' {content} -> content) (\s@Policy' {} a -> s {content = a} :: Policy)

instance Prelude.FromJSON Policy where
  parseJSON =
    Prelude.withObject
      "Policy"
      ( \x ->
          Policy'
            Prelude.<$> (x Prelude..:? "PolicySummary")
            Prelude.<*> (x Prelude..:? "Content")
      )

instance Prelude.Hashable Policy

instance Prelude.NFData Policy
