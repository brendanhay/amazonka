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
-- Module      : Network.AWS.EC2.Types.AddPrefixListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddPrefixListEntry where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An entry for a prefix list.
--
-- /See:/ 'newAddPrefixListEntry' smart constructor.
data AddPrefixListEntry = AddPrefixListEntry'
  { -- | A description for the entry.
    --
    -- Constraints: Up to 255 characters in length.
    description :: Prelude.Maybe Prelude.Text,
    -- | The CIDR block.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddPrefixListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'addPrefixListEntry_description' - A description for the entry.
--
-- Constraints: Up to 255 characters in length.
--
-- 'cidr', 'addPrefixListEntry_cidr' - The CIDR block.
newAddPrefixListEntry ::
  -- | 'cidr'
  Prelude.Text ->
  AddPrefixListEntry
newAddPrefixListEntry pCidr_ =
  AddPrefixListEntry'
    { description = Prelude.Nothing,
      cidr = pCidr_
    }

-- | A description for the entry.
--
-- Constraints: Up to 255 characters in length.
addPrefixListEntry_description :: Lens.Lens' AddPrefixListEntry (Prelude.Maybe Prelude.Text)
addPrefixListEntry_description = Lens.lens (\AddPrefixListEntry' {description} -> description) (\s@AddPrefixListEntry' {} a -> s {description = a} :: AddPrefixListEntry)

-- | The CIDR block.
addPrefixListEntry_cidr :: Lens.Lens' AddPrefixListEntry Prelude.Text
addPrefixListEntry_cidr = Lens.lens (\AddPrefixListEntry' {cidr} -> cidr) (\s@AddPrefixListEntry' {} a -> s {cidr = a} :: AddPrefixListEntry)

instance Prelude.Hashable AddPrefixListEntry

instance Prelude.NFData AddPrefixListEntry

instance Prelude.ToQuery AddPrefixListEntry where
  toQuery AddPrefixListEntry' {..} =
    Prelude.mconcat
      [ "Description" Prelude.=: description,
        "Cidr" Prelude.=: cidr
      ]
