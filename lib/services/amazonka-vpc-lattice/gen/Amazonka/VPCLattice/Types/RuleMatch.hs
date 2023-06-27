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
-- Module      : Amazonka.VPCLattice.Types.RuleMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.RuleMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.HttpMatch

-- | Describes a rule match.
--
-- /See:/ 'newRuleMatch' smart constructor.
data RuleMatch = RuleMatch'
  { -- | The HTTP criteria that a rule must match.
    httpMatch :: Prelude.Maybe HttpMatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMatch', 'ruleMatch_httpMatch' - The HTTP criteria that a rule must match.
newRuleMatch ::
  RuleMatch
newRuleMatch =
  RuleMatch' {httpMatch = Prelude.Nothing}

-- | The HTTP criteria that a rule must match.
ruleMatch_httpMatch :: Lens.Lens' RuleMatch (Prelude.Maybe HttpMatch)
ruleMatch_httpMatch = Lens.lens (\RuleMatch' {httpMatch} -> httpMatch) (\s@RuleMatch' {} a -> s {httpMatch = a} :: RuleMatch)

instance Data.FromJSON RuleMatch where
  parseJSON =
    Data.withObject
      "RuleMatch"
      ( \x ->
          RuleMatch' Prelude.<$> (x Data..:? "httpMatch")
      )

instance Prelude.Hashable RuleMatch where
  hashWithSalt _salt RuleMatch' {..} =
    _salt `Prelude.hashWithSalt` httpMatch

instance Prelude.NFData RuleMatch where
  rnf RuleMatch' {..} = Prelude.rnf httpMatch

instance Data.ToJSON RuleMatch where
  toJSON RuleMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [("httpMatch" Data..=) Prelude.<$> httpMatch]
      )
