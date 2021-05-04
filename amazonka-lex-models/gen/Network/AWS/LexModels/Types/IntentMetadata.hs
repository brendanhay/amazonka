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
-- Module      : Network.AWS.LexModels.Types.IntentMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.IntentMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about an intent.
--
-- /See:/ 'newIntentMetadata' smart constructor.
data IntentMetadata = IntentMetadata'
  { -- | The date that the intent was created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date that the intent was updated. When you create an intent, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The version of the intent.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the intent.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the intent.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'lastUpdatedDate', 'intentMetadata_lastUpdatedDate' - The date that the intent was updated. When you create an intent, the
-- creation date and last updated date are the same.
--
-- 'version', 'intentMetadata_version' - The version of the intent.
--
-- 'name', 'intentMetadata_name' - The name of the intent.
--
-- 'description', 'intentMetadata_description' - A description of the intent.
newIntentMetadata ::
  IntentMetadata
newIntentMetadata =
  IntentMetadata'
    { createdDate = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The date that the intent was created.
intentMetadata_createdDate :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.UTCTime)
intentMetadata_createdDate = Lens.lens (\IntentMetadata' {createdDate} -> createdDate) (\s@IntentMetadata' {} a -> s {createdDate = a} :: IntentMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The date that the intent was updated. When you create an intent, the
-- creation date and last updated date are the same.
intentMetadata_lastUpdatedDate :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.UTCTime)
intentMetadata_lastUpdatedDate = Lens.lens (\IntentMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@IntentMetadata' {} a -> s {lastUpdatedDate = a} :: IntentMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The version of the intent.
intentMetadata_version :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.Text)
intentMetadata_version = Lens.lens (\IntentMetadata' {version} -> version) (\s@IntentMetadata' {} a -> s {version = a} :: IntentMetadata)

-- | The name of the intent.
intentMetadata_name :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.Text)
intentMetadata_name = Lens.lens (\IntentMetadata' {name} -> name) (\s@IntentMetadata' {} a -> s {name = a} :: IntentMetadata)

-- | A description of the intent.
intentMetadata_description :: Lens.Lens' IntentMetadata (Prelude.Maybe Prelude.Text)
intentMetadata_description = Lens.lens (\IntentMetadata' {description} -> description) (\s@IntentMetadata' {} a -> s {description = a} :: IntentMetadata)

instance Prelude.FromJSON IntentMetadata where
  parseJSON =
    Prelude.withObject
      "IntentMetadata"
      ( \x ->
          IntentMetadata'
            Prelude.<$> (x Prelude..:? "createdDate")
            Prelude.<*> (x Prelude..:? "lastUpdatedDate")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable IntentMetadata

instance Prelude.NFData IntentMetadata
