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
-- Module      : Network.AWS.EC2.Types.PrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixList where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes prefixes for AWS services.
--
-- /See:/ 'newPrefixList' smart constructor.
data PrefixList = PrefixList'
  { -- | The name of the prefix.
    prefixListName :: Core.Maybe Core.Text,
    -- | The IP address range of the AWS service.
    cidrs :: Core.Maybe [Core.Text],
    -- | The ID of the prefix.
    prefixListId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixListName', 'prefixList_prefixListName' - The name of the prefix.
--
-- 'cidrs', 'prefixList_cidrs' - The IP address range of the AWS service.
--
-- 'prefixListId', 'prefixList_prefixListId' - The ID of the prefix.
newPrefixList ::
  PrefixList
newPrefixList =
  PrefixList'
    { prefixListName = Core.Nothing,
      cidrs = Core.Nothing,
      prefixListId = Core.Nothing
    }

-- | The name of the prefix.
prefixList_prefixListName :: Lens.Lens' PrefixList (Core.Maybe Core.Text)
prefixList_prefixListName = Lens.lens (\PrefixList' {prefixListName} -> prefixListName) (\s@PrefixList' {} a -> s {prefixListName = a} :: PrefixList)

-- | The IP address range of the AWS service.
prefixList_cidrs :: Lens.Lens' PrefixList (Core.Maybe [Core.Text])
prefixList_cidrs = Lens.lens (\PrefixList' {cidrs} -> cidrs) (\s@PrefixList' {} a -> s {cidrs = a} :: PrefixList) Core.. Lens.mapping Lens._Coerce

-- | The ID of the prefix.
prefixList_prefixListId :: Lens.Lens' PrefixList (Core.Maybe Core.Text)
prefixList_prefixListId = Lens.lens (\PrefixList' {prefixListId} -> prefixListId) (\s@PrefixList' {} a -> s {prefixListId = a} :: PrefixList)

instance Core.FromXML PrefixList where
  parseXML x =
    PrefixList'
      Core.<$> (x Core..@? "prefixListName")
      Core.<*> ( x Core..@? "cidrSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "prefixListId")

instance Core.Hashable PrefixList

instance Core.NFData PrefixList
