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
-- Module      : Amazonka.CostExplorer.Types.Group
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.MetricValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | One level of grouped data in the results.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The keys that are included in this group.
    keys :: Prelude.Maybe [Prelude.Text],
    -- | The metrics that are included in this group.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keys', 'group_keys' - The keys that are included in this group.
--
-- 'metrics', 'group_metrics' - The metrics that are included in this group.
newGroup ::
  Group
newGroup =
  Group'
    { keys = Prelude.Nothing,
      metrics = Prelude.Nothing
    }

-- | The keys that are included in this group.
group_keys :: Lens.Lens' Group (Prelude.Maybe [Prelude.Text])
group_keys = Lens.lens (\Group' {keys} -> keys) (\s@Group' {} a -> s {keys = a} :: Group) Prelude.. Lens.mapping Lens.coerced

-- | The metrics that are included in this group.
group_metrics :: Lens.Lens' Group (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue))
group_metrics = Lens.lens (\Group' {metrics} -> metrics) (\s@Group' {} a -> s {metrics = a} :: Group) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Group where
  parseJSON =
    Data.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Data..:? "Keys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Metrics" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` metrics

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf keys `Prelude.seq` Prelude.rnf metrics
