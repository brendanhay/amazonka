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
-- Module      : Amazonka.ElasticSearch.Types.ChangeProgressStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ChangeProgressStage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A progress stage details of a specific domain configuration change.
--
-- /See:/ 'newChangeProgressStage' smart constructor.
data ChangeProgressStage = ChangeProgressStage'
  { -- | The description of the progress stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last updated timestamp of the progress stage.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The name of the specific progress stage.
    name :: Prelude.Maybe Prelude.Text,
    -- | The overall status of a specific progress stage.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeProgressStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'changeProgressStage_description' - The description of the progress stage.
--
-- 'lastUpdated', 'changeProgressStage_lastUpdated' - The last updated timestamp of the progress stage.
--
-- 'name', 'changeProgressStage_name' - The name of the specific progress stage.
--
-- 'status', 'changeProgressStage_status' - The overall status of a specific progress stage.
newChangeProgressStage ::
  ChangeProgressStage
newChangeProgressStage =
  ChangeProgressStage'
    { description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The description of the progress stage.
changeProgressStage_description :: Lens.Lens' ChangeProgressStage (Prelude.Maybe Prelude.Text)
changeProgressStage_description = Lens.lens (\ChangeProgressStage' {description} -> description) (\s@ChangeProgressStage' {} a -> s {description = a} :: ChangeProgressStage)

-- | The last updated timestamp of the progress stage.
changeProgressStage_lastUpdated :: Lens.Lens' ChangeProgressStage (Prelude.Maybe Prelude.UTCTime)
changeProgressStage_lastUpdated = Lens.lens (\ChangeProgressStage' {lastUpdated} -> lastUpdated) (\s@ChangeProgressStage' {} a -> s {lastUpdated = a} :: ChangeProgressStage) Prelude.. Lens.mapping Data._Time

-- | The name of the specific progress stage.
changeProgressStage_name :: Lens.Lens' ChangeProgressStage (Prelude.Maybe Prelude.Text)
changeProgressStage_name = Lens.lens (\ChangeProgressStage' {name} -> name) (\s@ChangeProgressStage' {} a -> s {name = a} :: ChangeProgressStage)

-- | The overall status of a specific progress stage.
changeProgressStage_status :: Lens.Lens' ChangeProgressStage (Prelude.Maybe Prelude.Text)
changeProgressStage_status = Lens.lens (\ChangeProgressStage' {status} -> status) (\s@ChangeProgressStage' {} a -> s {status = a} :: ChangeProgressStage)

instance Data.FromJSON ChangeProgressStage where
  parseJSON =
    Data.withObject
      "ChangeProgressStage"
      ( \x ->
          ChangeProgressStage'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ChangeProgressStage where
  hashWithSalt _salt ChangeProgressStage' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChangeProgressStage where
  rnf ChangeProgressStage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
