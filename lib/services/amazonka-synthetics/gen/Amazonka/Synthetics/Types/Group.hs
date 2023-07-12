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
-- Module      : Amazonka.Synthetics.Types.Group
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about one group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The ARN of the group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the group was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The unique ID of the group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the group was most recently updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of key-value pairs that are associated with the canary.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'arn', 'group_arn' - The ARN of the group.
--
-- 'createdTime', 'group_createdTime' - The date and time that the group was created.
--
-- 'id', 'group_id' - The unique ID of the group.
--
-- 'lastModifiedTime', 'group_lastModifiedTime' - The date and time that the group was most recently updated.
--
-- 'name', 'group_name' - The name of the group.
--
-- 'tags', 'group_tags' - The list of key-value pairs that are associated with the canary.
newGroup ::
  Group
newGroup =
  Group'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the group.
group_arn :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_arn = Lens.lens (\Group' {arn} -> arn) (\s@Group' {} a -> s {arn = a} :: Group)

-- | The date and time that the group was created.
group_createdTime :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_createdTime = Lens.lens (\Group' {createdTime} -> createdTime) (\s@Group' {} a -> s {createdTime = a} :: Group) Prelude.. Lens.mapping Data._Time

-- | The unique ID of the group.
group_id :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_id = Lens.lens (\Group' {id} -> id) (\s@Group' {} a -> s {id = a} :: Group)

-- | The date and time that the group was most recently updated.
group_lastModifiedTime :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_lastModifiedTime = Lens.lens (\Group' {lastModifiedTime} -> lastModifiedTime) (\s@Group' {} a -> s {lastModifiedTime = a} :: Group) Prelude.. Lens.mapping Data._Time

-- | The name of the group.
group_name :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

-- | The list of key-value pairs that are associated with the canary.
group_tags :: Lens.Lens' Group (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
group_tags = Lens.lens (\Group' {tags} -> tags) (\s@Group' {} a -> s {tags = a} :: Group) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Group where
  parseJSON =
    Data.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
