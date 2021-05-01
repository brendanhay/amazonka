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
-- Module      : Network.AWS.WorkDocs.Types.GroupMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.GroupMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the metadata of a user group.
--
-- /See:/ 'newGroupMetadata' smart constructor.
data GroupMetadata = GroupMetadata'
  { -- | The ID of the user group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON GroupMetadata where
  parseJSON =
    Prelude.withObject
      "GroupMetadata"
      ( \x ->
          GroupMetadata'
            Prelude.<$> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable GroupMetadata

instance Prelude.NFData GroupMetadata
