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
-- Module      : Amazonka.Organizations.Types.Child
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.Child where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.ChildType
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of child entities, either OUs or accounts.
--
-- /See:/ 'newChild' smart constructor.
data Child = Child'
  { -- | The unique identifier (ID) of this child entity.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID
    -- string requires one of the following:
    --
    -- -   __Account__ - A string that consists of exactly 12 digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that contains the OU). This string is followed by a second
    --     \"-\" dash and from 8 to 32 additional lowercase letters or digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of this child entity.
    type' :: Prelude.Maybe ChildType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Child' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'child_id' - The unique identifier (ID) of this child entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID
-- string requires one of the following:
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that contains the OU). This string is followed by a second
--     \"-\" dash and from 8 to 32 additional lowercase letters or digits.
--
-- 'type'', 'child_type' - The type of this child entity.
newChild ::
  Child
newChild =
  Child'
    { id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The unique identifier (ID) of this child entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID
-- string requires one of the following:
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that contains the OU). This string is followed by a second
--     \"-\" dash and from 8 to 32 additional lowercase letters or digits.
child_id :: Lens.Lens' Child (Prelude.Maybe Prelude.Text)
child_id = Lens.lens (\Child' {id} -> id) (\s@Child' {} a -> s {id = a} :: Child)

-- | The type of this child entity.
child_type :: Lens.Lens' Child (Prelude.Maybe ChildType)
child_type = Lens.lens (\Child' {type'} -> type') (\s@Child' {} a -> s {type' = a} :: Child)

instance Data.FromJSON Child where
  parseJSON =
    Data.withObject
      "Child"
      ( \x ->
          Child'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Child where
  hashWithSalt _salt Child' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Child where
  rnf Child' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf type'
