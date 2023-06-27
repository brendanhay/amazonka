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
-- Module      : Amazonka.Lambda.Types.DocumentDBEventSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.DocumentDBEventSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.FullDocument
import qualified Amazonka.Prelude as Prelude

-- | Specific configuration settings for a DocumentDB event source.
--
-- /See:/ 'newDocumentDBEventSourceConfig' smart constructor.
data DocumentDBEventSourceConfig = DocumentDBEventSourceConfig'
  { -- | The name of the collection to consume within the database. If you do not
    -- specify a collection, Lambda consumes all collections.
    collectionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to consume within the DocumentDB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Determines what DocumentDB sends to your event stream during document
    -- update operations. If set to UpdateLookup, DocumentDB sends a delta
    -- describing the changes, along with a copy of the entire document.
    -- Otherwise, DocumentDB sends only a partial document that contains the
    -- changes.
    fullDocument :: Prelude.Maybe FullDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentDBEventSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'documentDBEventSourceConfig_collectionName' - The name of the collection to consume within the database. If you do not
-- specify a collection, Lambda consumes all collections.
--
-- 'databaseName', 'documentDBEventSourceConfig_databaseName' - The name of the database to consume within the DocumentDB cluster.
--
-- 'fullDocument', 'documentDBEventSourceConfig_fullDocument' - Determines what DocumentDB sends to your event stream during document
-- update operations. If set to UpdateLookup, DocumentDB sends a delta
-- describing the changes, along with a copy of the entire document.
-- Otherwise, DocumentDB sends only a partial document that contains the
-- changes.
newDocumentDBEventSourceConfig ::
  DocumentDBEventSourceConfig
newDocumentDBEventSourceConfig =
  DocumentDBEventSourceConfig'
    { collectionName =
        Prelude.Nothing,
      databaseName = Prelude.Nothing,
      fullDocument = Prelude.Nothing
    }

-- | The name of the collection to consume within the database. If you do not
-- specify a collection, Lambda consumes all collections.
documentDBEventSourceConfig_collectionName :: Lens.Lens' DocumentDBEventSourceConfig (Prelude.Maybe Prelude.Text)
documentDBEventSourceConfig_collectionName = Lens.lens (\DocumentDBEventSourceConfig' {collectionName} -> collectionName) (\s@DocumentDBEventSourceConfig' {} a -> s {collectionName = a} :: DocumentDBEventSourceConfig)

-- | The name of the database to consume within the DocumentDB cluster.
documentDBEventSourceConfig_databaseName :: Lens.Lens' DocumentDBEventSourceConfig (Prelude.Maybe Prelude.Text)
documentDBEventSourceConfig_databaseName = Lens.lens (\DocumentDBEventSourceConfig' {databaseName} -> databaseName) (\s@DocumentDBEventSourceConfig' {} a -> s {databaseName = a} :: DocumentDBEventSourceConfig)

-- | Determines what DocumentDB sends to your event stream during document
-- update operations. If set to UpdateLookup, DocumentDB sends a delta
-- describing the changes, along with a copy of the entire document.
-- Otherwise, DocumentDB sends only a partial document that contains the
-- changes.
documentDBEventSourceConfig_fullDocument :: Lens.Lens' DocumentDBEventSourceConfig (Prelude.Maybe FullDocument)
documentDBEventSourceConfig_fullDocument = Lens.lens (\DocumentDBEventSourceConfig' {fullDocument} -> fullDocument) (\s@DocumentDBEventSourceConfig' {} a -> s {fullDocument = a} :: DocumentDBEventSourceConfig)

instance Data.FromJSON DocumentDBEventSourceConfig where
  parseJSON =
    Data.withObject
      "DocumentDBEventSourceConfig"
      ( \x ->
          DocumentDBEventSourceConfig'
            Prelude.<$> (x Data..:? "CollectionName")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "FullDocument")
      )

instance Prelude.Hashable DocumentDBEventSourceConfig where
  hashWithSalt _salt DocumentDBEventSourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` fullDocument

instance Prelude.NFData DocumentDBEventSourceConfig where
  rnf DocumentDBEventSourceConfig' {..} =
    Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf fullDocument

instance Data.ToJSON DocumentDBEventSourceConfig where
  toJSON DocumentDBEventSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CollectionName" Data..=)
              Prelude.<$> collectionName,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("FullDocument" Data..=) Prelude.<$> fullDocument
          ]
      )
