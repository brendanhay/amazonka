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
-- Module      : Amazonka.EC2.Types.PathRequestFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathRequestFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.RequestFilterPortRange
import qualified Amazonka.Prelude as Prelude

-- | Describes a set of filters for a path analysis. Use path filters to
-- scope the analysis when there can be multiple resulting paths.
--
-- /See:/ 'newPathRequestFilter' smart constructor.
data PathRequestFilter = PathRequestFilter'
  { -- | The destination IPv4 address.
    destinationAddress :: Prelude.Maybe Prelude.Text,
    -- | The destination port range.
    destinationPortRange :: Prelude.Maybe RequestFilterPortRange,
    -- | The source IPv4 address.
    sourceAddress :: Prelude.Maybe Prelude.Text,
    -- | The source port range.
    sourcePortRange :: Prelude.Maybe RequestFilterPortRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathRequestFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationAddress', 'pathRequestFilter_destinationAddress' - The destination IPv4 address.
--
-- 'destinationPortRange', 'pathRequestFilter_destinationPortRange' - The destination port range.
--
-- 'sourceAddress', 'pathRequestFilter_sourceAddress' - The source IPv4 address.
--
-- 'sourcePortRange', 'pathRequestFilter_sourcePortRange' - The source port range.
newPathRequestFilter ::
  PathRequestFilter
newPathRequestFilter =
  PathRequestFilter'
    { destinationAddress =
        Prelude.Nothing,
      destinationPortRange = Prelude.Nothing,
      sourceAddress = Prelude.Nothing,
      sourcePortRange = Prelude.Nothing
    }

-- | The destination IPv4 address.
pathRequestFilter_destinationAddress :: Lens.Lens' PathRequestFilter (Prelude.Maybe Prelude.Text)
pathRequestFilter_destinationAddress = Lens.lens (\PathRequestFilter' {destinationAddress} -> destinationAddress) (\s@PathRequestFilter' {} a -> s {destinationAddress = a} :: PathRequestFilter)

-- | The destination port range.
pathRequestFilter_destinationPortRange :: Lens.Lens' PathRequestFilter (Prelude.Maybe RequestFilterPortRange)
pathRequestFilter_destinationPortRange = Lens.lens (\PathRequestFilter' {destinationPortRange} -> destinationPortRange) (\s@PathRequestFilter' {} a -> s {destinationPortRange = a} :: PathRequestFilter)

-- | The source IPv4 address.
pathRequestFilter_sourceAddress :: Lens.Lens' PathRequestFilter (Prelude.Maybe Prelude.Text)
pathRequestFilter_sourceAddress = Lens.lens (\PathRequestFilter' {sourceAddress} -> sourceAddress) (\s@PathRequestFilter' {} a -> s {sourceAddress = a} :: PathRequestFilter)

-- | The source port range.
pathRequestFilter_sourcePortRange :: Lens.Lens' PathRequestFilter (Prelude.Maybe RequestFilterPortRange)
pathRequestFilter_sourcePortRange = Lens.lens (\PathRequestFilter' {sourcePortRange} -> sourcePortRange) (\s@PathRequestFilter' {} a -> s {sourcePortRange = a} :: PathRequestFilter)

instance Prelude.Hashable PathRequestFilter where
  hashWithSalt _salt PathRequestFilter' {..} =
    _salt
      `Prelude.hashWithSalt` destinationAddress
      `Prelude.hashWithSalt` destinationPortRange
      `Prelude.hashWithSalt` sourceAddress
      `Prelude.hashWithSalt` sourcePortRange

instance Prelude.NFData PathRequestFilter where
  rnf PathRequestFilter' {..} =
    Prelude.rnf destinationAddress
      `Prelude.seq` Prelude.rnf destinationPortRange
      `Prelude.seq` Prelude.rnf sourceAddress
      `Prelude.seq` Prelude.rnf sourcePortRange

instance Data.ToQuery PathRequestFilter where
  toQuery PathRequestFilter' {..} =
    Prelude.mconcat
      [ "DestinationAddress" Data.=: destinationAddress,
        "DestinationPortRange" Data.=: destinationPortRange,
        "SourceAddress" Data.=: sourceAddress,
        "SourcePortRange" Data.=: sourcePortRange
      ]
