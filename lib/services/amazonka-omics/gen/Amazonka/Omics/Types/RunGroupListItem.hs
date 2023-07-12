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
-- Module      : Amazonka.Omics.Types.RunGroupListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.RunGroupListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A run group.
--
-- /See:/ 'newRunGroupListItem' smart constructor.
data RunGroupListItem = RunGroupListItem'
  { -- | The group\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the group was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The group\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The group\'s maximum CPU count setting.
    maxCpus :: Prelude.Maybe Prelude.Natural,
    -- | The group\'s maximum duration setting.
    maxDuration :: Prelude.Maybe Prelude.Natural,
    -- | The group\'s maximum concurrent run setting.
    maxRuns :: Prelude.Maybe Prelude.Natural,
    -- | The group\'s name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunGroupListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'runGroupListItem_arn' - The group\'s ARN.
--
-- 'creationTime', 'runGroupListItem_creationTime' - When the group was created.
--
-- 'id', 'runGroupListItem_id' - The group\'s ID.
--
-- 'maxCpus', 'runGroupListItem_maxCpus' - The group\'s maximum CPU count setting.
--
-- 'maxDuration', 'runGroupListItem_maxDuration' - The group\'s maximum duration setting.
--
-- 'maxRuns', 'runGroupListItem_maxRuns' - The group\'s maximum concurrent run setting.
--
-- 'name', 'runGroupListItem_name' - The group\'s name.
newRunGroupListItem ::
  RunGroupListItem
newRunGroupListItem =
  RunGroupListItem'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      id = Prelude.Nothing,
      maxCpus = Prelude.Nothing,
      maxDuration = Prelude.Nothing,
      maxRuns = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The group\'s ARN.
runGroupListItem_arn :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.Text)
runGroupListItem_arn = Lens.lens (\RunGroupListItem' {arn} -> arn) (\s@RunGroupListItem' {} a -> s {arn = a} :: RunGroupListItem)

-- | When the group was created.
runGroupListItem_creationTime :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.UTCTime)
runGroupListItem_creationTime = Lens.lens (\RunGroupListItem' {creationTime} -> creationTime) (\s@RunGroupListItem' {} a -> s {creationTime = a} :: RunGroupListItem) Prelude.. Lens.mapping Data._Time

-- | The group\'s ID.
runGroupListItem_id :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.Text)
runGroupListItem_id = Lens.lens (\RunGroupListItem' {id} -> id) (\s@RunGroupListItem' {} a -> s {id = a} :: RunGroupListItem)

-- | The group\'s maximum CPU count setting.
runGroupListItem_maxCpus :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.Natural)
runGroupListItem_maxCpus = Lens.lens (\RunGroupListItem' {maxCpus} -> maxCpus) (\s@RunGroupListItem' {} a -> s {maxCpus = a} :: RunGroupListItem)

-- | The group\'s maximum duration setting.
runGroupListItem_maxDuration :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.Natural)
runGroupListItem_maxDuration = Lens.lens (\RunGroupListItem' {maxDuration} -> maxDuration) (\s@RunGroupListItem' {} a -> s {maxDuration = a} :: RunGroupListItem)

-- | The group\'s maximum concurrent run setting.
runGroupListItem_maxRuns :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.Natural)
runGroupListItem_maxRuns = Lens.lens (\RunGroupListItem' {maxRuns} -> maxRuns) (\s@RunGroupListItem' {} a -> s {maxRuns = a} :: RunGroupListItem)

-- | The group\'s name.
runGroupListItem_name :: Lens.Lens' RunGroupListItem (Prelude.Maybe Prelude.Text)
runGroupListItem_name = Lens.lens (\RunGroupListItem' {name} -> name) (\s@RunGroupListItem' {} a -> s {name = a} :: RunGroupListItem)

instance Data.FromJSON RunGroupListItem where
  parseJSON =
    Data.withObject
      "RunGroupListItem"
      ( \x ->
          RunGroupListItem'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "maxCpus")
            Prelude.<*> (x Data..:? "maxDuration")
            Prelude.<*> (x Data..:? "maxRuns")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable RunGroupListItem where
  hashWithSalt _salt RunGroupListItem' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` maxCpus
      `Prelude.hashWithSalt` maxDuration
      `Prelude.hashWithSalt` maxRuns
      `Prelude.hashWithSalt` name

instance Prelude.NFData RunGroupListItem where
  rnf RunGroupListItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf maxCpus
      `Prelude.seq` Prelude.rnf maxDuration
      `Prelude.seq` Prelude.rnf maxRuns
      `Prelude.seq` Prelude.rnf name
