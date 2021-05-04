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
-- Module      : Network.AWS.CostExplorer.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Group where

import Network.AWS.CostExplorer.Types.MetricValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | One level of grouped data in the results.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The metrics that are included in this group.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue),
    -- | The keys that are included in this group.
    keys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'group_metrics' - The metrics that are included in this group.
--
-- 'keys', 'group_keys' - The keys that are included in this group.
newGroup ::
  Group
newGroup =
  Group'
    { metrics = Prelude.Nothing,
      keys = Prelude.Nothing
    }

-- | The metrics that are included in this group.
group_metrics :: Lens.Lens' Group (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue))
group_metrics = Lens.lens (\Group' {metrics} -> metrics) (\s@Group' {} a -> s {metrics = a} :: Group) Prelude.. Lens.mapping Prelude._Coerce

-- | The keys that are included in this group.
group_keys :: Lens.Lens' Group (Prelude.Maybe [Prelude.Text])
group_keys = Lens.lens (\Group' {keys} -> keys) (\s@Group' {} a -> s {keys = a} :: Group) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Group where
  parseJSON =
    Prelude.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Prelude..:? "Metrics" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Keys" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable Group

instance Prelude.NFData Group
