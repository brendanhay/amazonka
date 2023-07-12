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
-- Module      : Amazonka.WorkDocs.Types.GroupMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.GroupMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the metadata of a user group.
--
-- /See:/ 'newGroupMetadata' smart constructor.
data GroupMetadata = GroupMetadata'
  { -- | The ID of the user group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'groupMetadata_id' - The ID of the user group.
--
-- 'name', 'groupMetadata_name' - The name of the group.
newGroupMetadata ::
  GroupMetadata
newGroupMetadata =
  GroupMetadata'
    { id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ID of the user group.
groupMetadata_id :: Lens.Lens' GroupMetadata (Prelude.Maybe Prelude.Text)
groupMetadata_id = Lens.lens (\GroupMetadata' {id} -> id) (\s@GroupMetadata' {} a -> s {id = a} :: GroupMetadata)

-- | The name of the group.
groupMetadata_name :: Lens.Lens' GroupMetadata (Prelude.Maybe Prelude.Text)
groupMetadata_name = Lens.lens (\GroupMetadata' {name} -> name) (\s@GroupMetadata' {} a -> s {name = a} :: GroupMetadata)

instance Data.FromJSON GroupMetadata where
  parseJSON =
    Data.withObject
      "GroupMetadata"
      ( \x ->
          GroupMetadata'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable GroupMetadata where
  hashWithSalt _salt GroupMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData GroupMetadata where
  rnf GroupMetadata' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
