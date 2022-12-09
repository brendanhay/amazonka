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
-- Module      : Amazonka.LexModels.Types.IntentMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.IntentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an intent.
--
-- /See:/ 'newIntentMetadata' smart constructor.
data IntentMetadata = IntentMetadata'
  { -- | The date that the intent was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date that the intent was updated. When you create an intent, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the intent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the intent.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'intentMetadata_createdDate' - The date that the intent was created.
--
-- 'description', 'intentMetadata_description' - A description of the intent.
--
-- 'lastUpdatedDate', 'intentMetadata_lastUpdatedDate' - The date that the intent was updated. When you create an intent, the
-- creation date and last updated date are the same.
--
-- 'name', 'intentMetadata_name' - The name of the intent.
--
-- 'version', 'intentMetadata_version' - The version of the intent.
newIntentMetadata ::
  IntentMetadata
newIntentMetadata =
  IntentMetadata'
    { createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The date that the intent was created.
intentMetadata_createdDate :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.UTCTime)
intentMetadata_createdDate = Lens.lens (\IntentMetadata' {createdDate} -> createdDate) (\s@IntentMetadata' {} a -> s {createdDate = a} :: IntentMetadata) Prelude.. Lens.mapping Data._Time

-- | A description of the intent.
intentMetadata_description :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.Text)
intentMetadata_description = Lens.lens (\IntentMetadata' {description} -> description) (\s@IntentMetadata' {} a -> s {description = a} :: IntentMetadata)

-- | The date that the intent was updated. When you create an intent, the
-- creation date and last updated date are the same.
intentMetadata_lastUpdatedDate :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.UTCTime)
intentMetadata_lastUpdatedDate = Lens.lens (\IntentMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@IntentMetadata' {} a -> s {lastUpdatedDate = a} :: IntentMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the intent.
intentMetadata_name :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.Text)
intentMetadata_name = Lens.lens (\IntentMetadata' {name} -> name) (\s@IntentMetadata' {} a -> s {name = a} :: IntentMetadata)

-- | The version of the intent.
intentMetadata_version :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.Text)
intentMetadata_version = Lens.lens (\IntentMetadata' {version} -> version) (\s@IntentMetadata' {} a -> s {version = a} :: IntentMetadata)

instance Data.FromJSON IntentMetadata where
  parseJSON =
    Data.withObject
      "IntentMetadata"
      ( \x ->
          IntentMetadata'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable IntentMetadata where
  hashWithSalt _salt IntentMetadata' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData IntentMetadata where
  rnf IntentMetadata' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
