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
-- Module      : Amazonka.CustomerProfiles.Types.ListCalculatedAttributeDefinitionItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ListCalculatedAttributeDefinitionItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a single calculated attribute definition.
--
-- /See:/ 'newListCalculatedAttributeDefinitionItem' smart constructor.
data ListCalculatedAttributeDefinitionItem = ListCalculatedAttributeDefinitionItem'
  { -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The threshold for the calculated attribute.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The threshold for the calculated attribute.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the calculated attribute.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the calculated attribute definition was most
    -- recently edited.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCalculatedAttributeDefinitionItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculatedAttributeName', 'listCalculatedAttributeDefinitionItem_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'createdAt', 'listCalculatedAttributeDefinitionItem_createdAt' - The threshold for the calculated attribute.
--
-- 'description', 'listCalculatedAttributeDefinitionItem_description' - The threshold for the calculated attribute.
--
-- 'displayName', 'listCalculatedAttributeDefinitionItem_displayName' - The display name of the calculated attribute.
--
-- 'lastUpdatedAt', 'listCalculatedAttributeDefinitionItem_lastUpdatedAt' - The timestamp of when the calculated attribute definition was most
-- recently edited.
--
-- 'tags', 'listCalculatedAttributeDefinitionItem_tags' - The tags used to organize, track, or control access for this resource.
newListCalculatedAttributeDefinitionItem ::
  ListCalculatedAttributeDefinitionItem
newListCalculatedAttributeDefinitionItem =
  ListCalculatedAttributeDefinitionItem'
    { calculatedAttributeName =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The unique name of the calculated attribute.
listCalculatedAttributeDefinitionItem_calculatedAttributeName :: Lens.Lens' ListCalculatedAttributeDefinitionItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeDefinitionItem_calculatedAttributeName = Lens.lens (\ListCalculatedAttributeDefinitionItem' {calculatedAttributeName} -> calculatedAttributeName) (\s@ListCalculatedAttributeDefinitionItem' {} a -> s {calculatedAttributeName = a} :: ListCalculatedAttributeDefinitionItem)

-- | The threshold for the calculated attribute.
listCalculatedAttributeDefinitionItem_createdAt :: Lens.Lens' ListCalculatedAttributeDefinitionItem (Prelude.Maybe Prelude.UTCTime)
listCalculatedAttributeDefinitionItem_createdAt = Lens.lens (\ListCalculatedAttributeDefinitionItem' {createdAt} -> createdAt) (\s@ListCalculatedAttributeDefinitionItem' {} a -> s {createdAt = a} :: ListCalculatedAttributeDefinitionItem) Prelude.. Lens.mapping Data._Time

-- | The threshold for the calculated attribute.
listCalculatedAttributeDefinitionItem_description :: Lens.Lens' ListCalculatedAttributeDefinitionItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeDefinitionItem_description = Lens.lens (\ListCalculatedAttributeDefinitionItem' {description} -> description) (\s@ListCalculatedAttributeDefinitionItem' {} a -> s {description = a} :: ListCalculatedAttributeDefinitionItem)

-- | The display name of the calculated attribute.
listCalculatedAttributeDefinitionItem_displayName :: Lens.Lens' ListCalculatedAttributeDefinitionItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeDefinitionItem_displayName = Lens.lens (\ListCalculatedAttributeDefinitionItem' {displayName} -> displayName) (\s@ListCalculatedAttributeDefinitionItem' {} a -> s {displayName = a} :: ListCalculatedAttributeDefinitionItem)

-- | The timestamp of when the calculated attribute definition was most
-- recently edited.
listCalculatedAttributeDefinitionItem_lastUpdatedAt :: Lens.Lens' ListCalculatedAttributeDefinitionItem (Prelude.Maybe Prelude.UTCTime)
listCalculatedAttributeDefinitionItem_lastUpdatedAt = Lens.lens (\ListCalculatedAttributeDefinitionItem' {lastUpdatedAt} -> lastUpdatedAt) (\s@ListCalculatedAttributeDefinitionItem' {} a -> s {lastUpdatedAt = a} :: ListCalculatedAttributeDefinitionItem) Prelude.. Lens.mapping Data._Time

-- | The tags used to organize, track, or control access for this resource.
listCalculatedAttributeDefinitionItem_tags :: Lens.Lens' ListCalculatedAttributeDefinitionItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listCalculatedAttributeDefinitionItem_tags = Lens.lens (\ListCalculatedAttributeDefinitionItem' {tags} -> tags) (\s@ListCalculatedAttributeDefinitionItem' {} a -> s {tags = a} :: ListCalculatedAttributeDefinitionItem) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    ListCalculatedAttributeDefinitionItem
  where
  parseJSON =
    Data.withObject
      "ListCalculatedAttributeDefinitionItem"
      ( \x ->
          ListCalculatedAttributeDefinitionItem'
            Prelude.<$> (x Data..:? "CalculatedAttributeName")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListCalculatedAttributeDefinitionItem
  where
  hashWithSalt
    _salt
    ListCalculatedAttributeDefinitionItem' {..} =
      _salt
        `Prelude.hashWithSalt` calculatedAttributeName
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` displayName
        `Prelude.hashWithSalt` lastUpdatedAt
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    ListCalculatedAttributeDefinitionItem
  where
  rnf ListCalculatedAttributeDefinitionItem' {..} =
    Prelude.rnf calculatedAttributeName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf tags
