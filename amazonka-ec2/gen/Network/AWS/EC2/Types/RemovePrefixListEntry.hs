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
-- Module      : Network.AWS.EC2.Types.RemovePrefixListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RemovePrefixListEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | An entry for a prefix list.
--
-- /See:/ 'newRemovePrefixListEntry' smart constructor.
data RemovePrefixListEntry = RemovePrefixListEntry'
  { -- | The CIDR block.
    cidr :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemovePrefixListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'removePrefixListEntry_cidr' - The CIDR block.
newRemovePrefixListEntry ::
  -- | 'cidr'
  Core.Text ->
  RemovePrefixListEntry
newRemovePrefixListEntry pCidr_ =
  RemovePrefixListEntry' {cidr = pCidr_}

-- | The CIDR block.
removePrefixListEntry_cidr :: Lens.Lens' RemovePrefixListEntry Core.Text
removePrefixListEntry_cidr = Lens.lens (\RemovePrefixListEntry' {cidr} -> cidr) (\s@RemovePrefixListEntry' {} a -> s {cidr = a} :: RemovePrefixListEntry)

instance Core.Hashable RemovePrefixListEntry

instance Core.NFData RemovePrefixListEntry

instance Core.ToQuery RemovePrefixListEntry where
  toQuery RemovePrefixListEntry' {..} =
    Core.mconcat ["Cidr" Core.=: cidr]
