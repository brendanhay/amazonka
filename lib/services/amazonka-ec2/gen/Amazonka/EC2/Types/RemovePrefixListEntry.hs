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
-- Module      : Amazonka.EC2.Types.RemovePrefixListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RemovePrefixListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | An entry for a prefix list.
--
-- /See:/ 'newRemovePrefixListEntry' smart constructor.
data RemovePrefixListEntry = RemovePrefixListEntry'
  { -- | The CIDR block.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RemovePrefixListEntry
newRemovePrefixListEntry pCidr_ =
  RemovePrefixListEntry' {cidr = pCidr_}

-- | The CIDR block.
removePrefixListEntry_cidr :: Lens.Lens' RemovePrefixListEntry Prelude.Text
removePrefixListEntry_cidr = Lens.lens (\RemovePrefixListEntry' {cidr} -> cidr) (\s@RemovePrefixListEntry' {} a -> s {cidr = a} :: RemovePrefixListEntry)

instance Prelude.Hashable RemovePrefixListEntry where
  hashWithSalt _salt RemovePrefixListEntry' {..} =
    _salt `Prelude.hashWithSalt` cidr

instance Prelude.NFData RemovePrefixListEntry where
  rnf RemovePrefixListEntry' {..} = Prelude.rnf cidr

instance Data.ToQuery RemovePrefixListEntry where
  toQuery RemovePrefixListEntry' {..} =
    Prelude.mconcat ["Cidr" Data.=: cidr]
