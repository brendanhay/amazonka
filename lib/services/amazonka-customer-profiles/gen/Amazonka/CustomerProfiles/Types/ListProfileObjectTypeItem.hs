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
-- Module      : Amazonka.CustomerProfiles.Types.ListProfileObjectTypeItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ListProfileObjectTypeItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A ProfileObjectType instance.
--
-- /See:/ 'newListProfileObjectTypeItem' smart constructor.
data ListProfileObjectTypeItem = ListProfileObjectTypeItem'
  { -- | The timestamp of when the domain was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | Description of the profile object type.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjectTypeItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'listProfileObjectTypeItem_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'listProfileObjectTypeItem_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
--
-- 'tags', 'listProfileObjectTypeItem_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'objectTypeName', 'listProfileObjectTypeItem_objectTypeName' - The name of the profile object type.
--
-- 'description', 'listProfileObjectTypeItem_description' - Description of the profile object type.
newListProfileObjectTypeItem ::
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  ListProfileObjectTypeItem
newListProfileObjectTypeItem
  pObjectTypeName_
  pDescription_ =
    ListProfileObjectTypeItem'
      { createdAt =
          Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        tags = Prelude.Nothing,
        objectTypeName = pObjectTypeName_,
        description = pDescription_
      }

-- | The timestamp of when the domain was created.
listProfileObjectTypeItem_createdAt :: Lens.Lens' ListProfileObjectTypeItem (Prelude.Maybe Prelude.UTCTime)
listProfileObjectTypeItem_createdAt = Lens.lens (\ListProfileObjectTypeItem' {createdAt} -> createdAt) (\s@ListProfileObjectTypeItem' {} a -> s {createdAt = a} :: ListProfileObjectTypeItem) Prelude.. Lens.mapping Data._Time

-- | The timestamp of when the domain was most recently edited.
listProfileObjectTypeItem_lastUpdatedAt :: Lens.Lens' ListProfileObjectTypeItem (Prelude.Maybe Prelude.UTCTime)
listProfileObjectTypeItem_lastUpdatedAt = Lens.lens (\ListProfileObjectTypeItem' {lastUpdatedAt} -> lastUpdatedAt) (\s@ListProfileObjectTypeItem' {} a -> s {lastUpdatedAt = a} :: ListProfileObjectTypeItem) Prelude.. Lens.mapping Data._Time

-- | The tags used to organize, track, or control access for this resource.
listProfileObjectTypeItem_tags :: Lens.Lens' ListProfileObjectTypeItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listProfileObjectTypeItem_tags = Lens.lens (\ListProfileObjectTypeItem' {tags} -> tags) (\s@ListProfileObjectTypeItem' {} a -> s {tags = a} :: ListProfileObjectTypeItem) Prelude.. Lens.mapping Lens.coerced

-- | The name of the profile object type.
listProfileObjectTypeItem_objectTypeName :: Lens.Lens' ListProfileObjectTypeItem Prelude.Text
listProfileObjectTypeItem_objectTypeName = Lens.lens (\ListProfileObjectTypeItem' {objectTypeName} -> objectTypeName) (\s@ListProfileObjectTypeItem' {} a -> s {objectTypeName = a} :: ListProfileObjectTypeItem)

-- | Description of the profile object type.
listProfileObjectTypeItem_description :: Lens.Lens' ListProfileObjectTypeItem Prelude.Text
listProfileObjectTypeItem_description = Lens.lens (\ListProfileObjectTypeItem' {description} -> description) (\s@ListProfileObjectTypeItem' {} a -> s {description = a} :: ListProfileObjectTypeItem)

instance Data.FromJSON ListProfileObjectTypeItem where
  parseJSON =
    Data.withObject
      "ListProfileObjectTypeItem"
      ( \x ->
          ListProfileObjectTypeItem'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ObjectTypeName")
            Prelude.<*> (x Data..: "Description")
      )

instance Prelude.Hashable ListProfileObjectTypeItem where
  hashWithSalt _salt ListProfileObjectTypeItem' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` description

instance Prelude.NFData ListProfileObjectTypeItem where
  rnf ListProfileObjectTypeItem' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf description
