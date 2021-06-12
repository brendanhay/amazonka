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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | An entry for a prefix list.
--
-- /See:/ 'newAddPrefixListEntry' smart constructor.
data AddPrefixListEntry = AddPrefixListEntry'
  { -- | A description for the entry.
    --
    -- Constraints: Up to 255 characters in length.
    description :: Core.Maybe Core.Text,
    -- | The CIDR block.
    cidr :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  AddPrefixListEntry
newAddPrefixListEntry pCidr_ =
  AddPrefixListEntry'
    { description = Core.Nothing,
      cidr = pCidr_
    }

-- | A description for the entry.
--
-- Constraints: Up to 255 characters in length.
addPrefixListEntry_description :: Lens.Lens' AddPrefixListEntry (Core.Maybe Core.Text)
addPrefixListEntry_description = Lens.lens (\AddPrefixListEntry' {description} -> description) (\s@AddPrefixListEntry' {} a -> s {description = a} :: AddPrefixListEntry)

-- | The CIDR block.
addPrefixListEntry_cidr :: Lens.Lens' AddPrefixListEntry Core.Text
addPrefixListEntry_cidr = Lens.lens (\AddPrefixListEntry' {cidr} -> cidr) (\s@AddPrefixListEntry' {} a -> s {cidr = a} :: AddPrefixListEntry)

instance Core.Hashable AddPrefixListEntry

instance Core.NFData AddPrefixListEntry

instance Core.ToQuery AddPrefixListEntry where
  toQuery AddPrefixListEntry' {..} =
    Core.mconcat
      [ "Description" Core.=: description,
        "Cidr" Core.=: cidr
      ]
