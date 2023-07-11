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
-- Module      : Amazonka.LakeFormation.Types.TaggedDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.TaggedDatabase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.DatabaseResource
import Amazonka.LakeFormation.Types.LFTagPair
import qualified Amazonka.Prelude as Prelude

-- | A structure describing a database resource with LF-tags.
--
-- /See:/ 'newTaggedDatabase' smart constructor.
data TaggedDatabase = TaggedDatabase'
  { -- | A database that has LF-tags attached to it.
    database :: Prelude.Maybe DatabaseResource,
    -- | A list of LF-tags attached to the database.
    lFTags :: Prelude.Maybe (Prelude.NonEmpty LFTagPair)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaggedDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'database', 'taggedDatabase_database' - A database that has LF-tags attached to it.
--
-- 'lFTags', 'taggedDatabase_lFTags' - A list of LF-tags attached to the database.
newTaggedDatabase ::
  TaggedDatabase
newTaggedDatabase =
  TaggedDatabase'
    { database = Prelude.Nothing,
      lFTags = Prelude.Nothing
    }

-- | A database that has LF-tags attached to it.
taggedDatabase_database :: Lens.Lens' TaggedDatabase (Prelude.Maybe DatabaseResource)
taggedDatabase_database = Lens.lens (\TaggedDatabase' {database} -> database) (\s@TaggedDatabase' {} a -> s {database = a} :: TaggedDatabase)

-- | A list of LF-tags attached to the database.
taggedDatabase_lFTags :: Lens.Lens' TaggedDatabase (Prelude.Maybe (Prelude.NonEmpty LFTagPair))
taggedDatabase_lFTags = Lens.lens (\TaggedDatabase' {lFTags} -> lFTags) (\s@TaggedDatabase' {} a -> s {lFTags = a} :: TaggedDatabase) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TaggedDatabase where
  parseJSON =
    Data.withObject
      "TaggedDatabase"
      ( \x ->
          TaggedDatabase'
            Prelude.<$> (x Data..:? "Database")
            Prelude.<*> (x Data..:? "LFTags")
      )

instance Prelude.Hashable TaggedDatabase where
  hashWithSalt _salt TaggedDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` lFTags

instance Prelude.NFData TaggedDatabase where
  rnf TaggedDatabase' {..} =
    Prelude.rnf database
      `Prelude.seq` Prelude.rnf lFTags
