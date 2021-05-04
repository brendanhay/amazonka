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
-- Module      : Network.AWS.EC2.Types.PrefixListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListEntry where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a prefix list entry.
--
-- /See:/ 'newPrefixListEntry' smart constructor.
data PrefixListEntry = PrefixListEntry'
  { -- | The CIDR block.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PrefixListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'prefixListEntry_cidr' - The CIDR block.
--
-- 'description', 'prefixListEntry_description' - The description.
newPrefixListEntry ::
  PrefixListEntry
newPrefixListEntry =
  PrefixListEntry'
    { cidr = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The CIDR block.
prefixListEntry_cidr :: Lens.Lens' PrefixListEntry (Prelude.Maybe Prelude.Text)
prefixListEntry_cidr = Lens.lens (\PrefixListEntry' {cidr} -> cidr) (\s@PrefixListEntry' {} a -> s {cidr = a} :: PrefixListEntry)

-- | The description.
prefixListEntry_description :: Lens.Lens' PrefixListEntry (Prelude.Maybe Prelude.Text)
prefixListEntry_description = Lens.lens (\PrefixListEntry' {description} -> description) (\s@PrefixListEntry' {} a -> s {description = a} :: PrefixListEntry)

instance Prelude.FromXML PrefixListEntry where
  parseXML x =
    PrefixListEntry'
      Prelude.<$> (x Prelude..@? "cidr")
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable PrefixListEntry

instance Prelude.NFData PrefixListEntry
