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
-- Module      : Amazonka.EC2.Types.PathFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FilterPortRange
import qualified Amazonka.Prelude as Prelude

-- | Describes a set of filters for a path analysis. Use path filters to
-- scope the analysis when there can be multiple resulting paths.
--
-- /See:/ 'newPathFilter' smart constructor.
data PathFilter = PathFilter'
  { -- | The destination IPv4 address.
    destinationAddress :: Prelude.Maybe Prelude.Text,
    -- | The destination port range.
    destinationPortRange :: Prelude.Maybe FilterPortRange,
    -- | The source IPv4 address.
    sourceAddress :: Prelude.Maybe Prelude.Text,
    -- | The source port range.
    sourcePortRange :: Prelude.Maybe FilterPortRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationAddress', 'pathFilter_destinationAddress' - The destination IPv4 address.
--
-- 'destinationPortRange', 'pathFilter_destinationPortRange' - The destination port range.
--
-- 'sourceAddress', 'pathFilter_sourceAddress' - The source IPv4 address.
--
-- 'sourcePortRange', 'pathFilter_sourcePortRange' - The source port range.
newPathFilter ::
  PathFilter
newPathFilter =
  PathFilter'
    { destinationAddress = Prelude.Nothing,
      destinationPortRange = Prelude.Nothing,
      sourceAddress = Prelude.Nothing,
      sourcePortRange = Prelude.Nothing
    }

-- | The destination IPv4 address.
pathFilter_destinationAddress :: Lens.Lens' PathFilter (Prelude.Maybe Prelude.Text)
pathFilter_destinationAddress = Lens.lens (\PathFilter' {destinationAddress} -> destinationAddress) (\s@PathFilter' {} a -> s {destinationAddress = a} :: PathFilter)

-- | The destination port range.
pathFilter_destinationPortRange :: Lens.Lens' PathFilter (Prelude.Maybe FilterPortRange)
pathFilter_destinationPortRange = Lens.lens (\PathFilter' {destinationPortRange} -> destinationPortRange) (\s@PathFilter' {} a -> s {destinationPortRange = a} :: PathFilter)

-- | The source IPv4 address.
pathFilter_sourceAddress :: Lens.Lens' PathFilter (Prelude.Maybe Prelude.Text)
pathFilter_sourceAddress = Lens.lens (\PathFilter' {sourceAddress} -> sourceAddress) (\s@PathFilter' {} a -> s {sourceAddress = a} :: PathFilter)

-- | The source port range.
pathFilter_sourcePortRange :: Lens.Lens' PathFilter (Prelude.Maybe FilterPortRange)
pathFilter_sourcePortRange = Lens.lens (\PathFilter' {sourcePortRange} -> sourcePortRange) (\s@PathFilter' {} a -> s {sourcePortRange = a} :: PathFilter)

instance Data.FromXML PathFilter where
  parseXML x =
    PathFilter'
      Prelude.<$> (x Data..@? "destinationAddress")
      Prelude.<*> (x Data..@? "destinationPortRange")
      Prelude.<*> (x Data..@? "sourceAddress")
      Prelude.<*> (x Data..@? "sourcePortRange")

instance Prelude.Hashable PathFilter where
  hashWithSalt _salt PathFilter' {..} =
    _salt
      `Prelude.hashWithSalt` destinationAddress
      `Prelude.hashWithSalt` destinationPortRange
      `Prelude.hashWithSalt` sourceAddress
      `Prelude.hashWithSalt` sourcePortRange

instance Prelude.NFData PathFilter where
  rnf PathFilter' {..} =
    Prelude.rnf destinationAddress
      `Prelude.seq` Prelude.rnf destinationPortRange
      `Prelude.seq` Prelude.rnf sourceAddress
      `Prelude.seq` Prelude.rnf sourcePortRange
