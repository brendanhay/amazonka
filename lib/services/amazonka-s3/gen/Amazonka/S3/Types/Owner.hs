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
-- Module      : Amazonka.S3.Types.Owner
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Owner where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Container for the owner\'s display name and ID.
--
-- /See:/ 'newOwner' smart constructor.
data Owner = Owner'
  { -- | Container for the display name of the owner.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Container for the ID of the owner.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Owner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'owner_displayName' - Container for the display name of the owner.
--
-- 'id', 'owner_id' - Container for the ID of the owner.
newOwner ::
  Owner
newOwner =
  Owner'
    { displayName = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Container for the display name of the owner.
owner_displayName :: Lens.Lens' Owner (Prelude.Maybe Prelude.Text)
owner_displayName = Lens.lens (\Owner' {displayName} -> displayName) (\s@Owner' {} a -> s {displayName = a} :: Owner)

-- | Container for the ID of the owner.
owner_id :: Lens.Lens' Owner (Prelude.Maybe Prelude.Text)
owner_id = Lens.lens (\Owner' {id} -> id) (\s@Owner' {} a -> s {id = a} :: Owner)

instance Data.FromXML Owner where
  parseXML x =
    Owner'
      Prelude.<$> (x Data..@? "DisplayName")
      Prelude.<*> (x Data..@? "ID")

instance Prelude.Hashable Owner where
  hashWithSalt _salt Owner' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` id

instance Prelude.NFData Owner where
  rnf Owner' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf id

instance Data.ToXML Owner where
  toXML Owner' {..} =
    Prelude.mconcat
      ["DisplayName" Data.@= displayName, "ID" Data.@= id]
