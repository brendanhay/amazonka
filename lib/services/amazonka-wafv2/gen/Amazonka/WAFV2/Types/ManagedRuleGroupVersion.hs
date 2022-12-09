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
-- Module      : Amazonka.WAFV2.Types.ManagedRuleGroupVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleGroupVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a single version of a managed rule group.
--
-- /See:/ 'newManagedRuleGroupVersion' smart constructor.
data ManagedRuleGroupVersion = ManagedRuleGroupVersion'
  { -- | The date and time that the managed rule group owner updated the rule
    -- group version information.
    lastUpdateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The version name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleGroupVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTimestamp', 'managedRuleGroupVersion_lastUpdateTimestamp' - The date and time that the managed rule group owner updated the rule
-- group version information.
--
-- 'name', 'managedRuleGroupVersion_name' - The version name.
newManagedRuleGroupVersion ::
  ManagedRuleGroupVersion
newManagedRuleGroupVersion =
  ManagedRuleGroupVersion'
    { lastUpdateTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The date and time that the managed rule group owner updated the rule
-- group version information.
managedRuleGroupVersion_lastUpdateTimestamp :: Lens.Lens' ManagedRuleGroupVersion (Prelude.Maybe Prelude.UTCTime)
managedRuleGroupVersion_lastUpdateTimestamp = Lens.lens (\ManagedRuleGroupVersion' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@ManagedRuleGroupVersion' {} a -> s {lastUpdateTimestamp = a} :: ManagedRuleGroupVersion) Prelude.. Lens.mapping Data._Time

-- | The version name.
managedRuleGroupVersion_name :: Lens.Lens' ManagedRuleGroupVersion (Prelude.Maybe Prelude.Text)
managedRuleGroupVersion_name = Lens.lens (\ManagedRuleGroupVersion' {name} -> name) (\s@ManagedRuleGroupVersion' {} a -> s {name = a} :: ManagedRuleGroupVersion)

instance Data.FromJSON ManagedRuleGroupVersion where
  parseJSON =
    Data.withObject
      "ManagedRuleGroupVersion"
      ( \x ->
          ManagedRuleGroupVersion'
            Prelude.<$> (x Data..:? "LastUpdateTimestamp")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ManagedRuleGroupVersion where
  hashWithSalt _salt ManagedRuleGroupVersion' {..} =
    _salt `Prelude.hashWithSalt` lastUpdateTimestamp
      `Prelude.hashWithSalt` name

instance Prelude.NFData ManagedRuleGroupVersion where
  rnf ManagedRuleGroupVersion' {..} =
    Prelude.rnf lastUpdateTimestamp
      `Prelude.seq` Prelude.rnf name
