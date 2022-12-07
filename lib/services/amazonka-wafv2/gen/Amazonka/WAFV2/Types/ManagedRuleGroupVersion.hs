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
  { -- | The version name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the managed rule group owner updated the rule
    -- group version information.
    lastUpdateTimestamp :: Prelude.Maybe Data.POSIX
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
-- 'name', 'managedRuleGroupVersion_name' - The version name.
--
-- 'lastUpdateTimestamp', 'managedRuleGroupVersion_lastUpdateTimestamp' - The date and time that the managed rule group owner updated the rule
-- group version information.
newManagedRuleGroupVersion ::
  ManagedRuleGroupVersion
newManagedRuleGroupVersion =
  ManagedRuleGroupVersion'
    { name = Prelude.Nothing,
      lastUpdateTimestamp = Prelude.Nothing
    }

-- | The version name.
managedRuleGroupVersion_name :: Lens.Lens' ManagedRuleGroupVersion (Prelude.Maybe Prelude.Text)
managedRuleGroupVersion_name = Lens.lens (\ManagedRuleGroupVersion' {name} -> name) (\s@ManagedRuleGroupVersion' {} a -> s {name = a} :: ManagedRuleGroupVersion)

-- | The date and time that the managed rule group owner updated the rule
-- group version information.
managedRuleGroupVersion_lastUpdateTimestamp :: Lens.Lens' ManagedRuleGroupVersion (Prelude.Maybe Prelude.UTCTime)
managedRuleGroupVersion_lastUpdateTimestamp = Lens.lens (\ManagedRuleGroupVersion' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@ManagedRuleGroupVersion' {} a -> s {lastUpdateTimestamp = a} :: ManagedRuleGroupVersion) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ManagedRuleGroupVersion where
  parseJSON =
    Data.withObject
      "ManagedRuleGroupVersion"
      ( \x ->
          ManagedRuleGroupVersion'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "LastUpdateTimestamp")
      )

instance Prelude.Hashable ManagedRuleGroupVersion where
  hashWithSalt _salt ManagedRuleGroupVersion' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdateTimestamp

instance Prelude.NFData ManagedRuleGroupVersion where
  rnf ManagedRuleGroupVersion' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdateTimestamp
