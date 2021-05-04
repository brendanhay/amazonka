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
-- Module      : Network.AWS.S3.Types.Owner
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Owner where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container for the owner\'s display name and ID.
--
-- /See:/ 'newOwner' smart constructor.
data Owner = Owner'
  { -- | Container for the ID of the owner.
    id :: Prelude.Maybe Prelude.Text,
    -- | Container for the display name of the owner.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Owner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'owner_id' - Container for the ID of the owner.
--
-- 'displayName', 'owner_displayName' - Container for the display name of the owner.
newOwner ::
  Owner
newOwner =
  Owner'
    { id = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | Container for the ID of the owner.
owner_id :: Lens.Lens' Owner (Prelude.Maybe Prelude.Text)
owner_id = Lens.lens (\Owner' {id} -> id) (\s@Owner' {} a -> s {id = a} :: Owner)

-- | Container for the display name of the owner.
owner_displayName :: Lens.Lens' Owner (Prelude.Maybe Prelude.Text)
owner_displayName = Lens.lens (\Owner' {displayName} -> displayName) (\s@Owner' {} a -> s {displayName = a} :: Owner)

instance Prelude.FromXML Owner where
  parseXML x =
    Owner'
      Prelude.<$> (x Prelude..@? "ID")
      Prelude.<*> (x Prelude..@? "DisplayName")

instance Prelude.Hashable Owner

instance Prelude.NFData Owner

instance Prelude.ToXML Owner where
  toXML Owner' {..} =
    Prelude.mconcat
      [ "ID" Prelude.@= id,
        "DisplayName" Prelude.@= displayName
      ]
