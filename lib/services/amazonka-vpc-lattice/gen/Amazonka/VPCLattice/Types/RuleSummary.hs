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
-- Module      : Amazonka.VPCLattice.Types.RuleSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.RuleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about the listener rule.
--
-- /See:/ 'newRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the listener rule was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the rule.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default rule. Listener rules are created
    -- when you create a listener. Each listener has a default rule for
    -- checking connection requests.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The date and time that the listener rule was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The priority of the rule.
    priority :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'ruleSummary_arn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'createdAt', 'ruleSummary_createdAt' - The date and time that the listener rule was created, specified in
-- ISO-8601 format.
--
-- 'id', 'ruleSummary_id' - The ID of the rule.
--
-- 'isDefault', 'ruleSummary_isDefault' - Indicates whether this is the default rule. Listener rules are created
-- when you create a listener. Each listener has a default rule for
-- checking connection requests.
--
-- 'lastUpdatedAt', 'ruleSummary_lastUpdatedAt' - The date and time that the listener rule was last updated, specified in
-- ISO-8601 format.
--
-- 'name', 'ruleSummary_name' - The name of the rule.
--
-- 'priority', 'ruleSummary_priority' - The priority of the rule.
newRuleSummary ::
  RuleSummary
newRuleSummary =
  RuleSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the rule.
ruleSummary_arn :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_arn = Lens.lens (\RuleSummary' {arn} -> arn) (\s@RuleSummary' {} a -> s {arn = a} :: RuleSummary)

-- | The date and time that the listener rule was created, specified in
-- ISO-8601 format.
ruleSummary_createdAt :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.UTCTime)
ruleSummary_createdAt = Lens.lens (\RuleSummary' {createdAt} -> createdAt) (\s@RuleSummary' {} a -> s {createdAt = a} :: RuleSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the rule.
ruleSummary_id :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_id = Lens.lens (\RuleSummary' {id} -> id) (\s@RuleSummary' {} a -> s {id = a} :: RuleSummary)

-- | Indicates whether this is the default rule. Listener rules are created
-- when you create a listener. Each listener has a default rule for
-- checking connection requests.
ruleSummary_isDefault :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Bool)
ruleSummary_isDefault = Lens.lens (\RuleSummary' {isDefault} -> isDefault) (\s@RuleSummary' {} a -> s {isDefault = a} :: RuleSummary)

-- | The date and time that the listener rule was last updated, specified in
-- ISO-8601 format.
ruleSummary_lastUpdatedAt :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.UTCTime)
ruleSummary_lastUpdatedAt = Lens.lens (\RuleSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@RuleSummary' {} a -> s {lastUpdatedAt = a} :: RuleSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the rule.
ruleSummary_name :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_name = Lens.lens (\RuleSummary' {name} -> name) (\s@RuleSummary' {} a -> s {name = a} :: RuleSummary)

-- | The priority of the rule.
ruleSummary_priority :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Natural)
ruleSummary_priority = Lens.lens (\RuleSummary' {priority} -> priority) (\s@RuleSummary' {} a -> s {priority = a} :: RuleSummary)

instance Data.FromJSON RuleSummary where
  parseJSON =
    Data.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "isDefault")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "priority")
      )

instance Prelude.Hashable RuleSummary where
  hashWithSalt _salt RuleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority

instance Prelude.NFData RuleSummary where
  rnf RuleSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
