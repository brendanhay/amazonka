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
-- Module      : Amazonka.Shield.Types.ProtectionGroupPatternTypeLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroupPatternTypeLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ProtectionGroupArbitraryPatternLimits

-- | Limits settings by pattern type in the protection groups for your
-- subscription.
--
-- /See:/ 'newProtectionGroupPatternTypeLimits' smart constructor.
data ProtectionGroupPatternTypeLimits = ProtectionGroupPatternTypeLimits'
  { -- | Limits settings on protection groups with arbitrary pattern type.
    arbitraryPatternLimits :: ProtectionGroupArbitraryPatternLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectionGroupPatternTypeLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arbitraryPatternLimits', 'protectionGroupPatternTypeLimits_arbitraryPatternLimits' - Limits settings on protection groups with arbitrary pattern type.
newProtectionGroupPatternTypeLimits ::
  -- | 'arbitraryPatternLimits'
  ProtectionGroupArbitraryPatternLimits ->
  ProtectionGroupPatternTypeLimits
newProtectionGroupPatternTypeLimits
  pArbitraryPatternLimits_ =
    ProtectionGroupPatternTypeLimits'
      { arbitraryPatternLimits =
          pArbitraryPatternLimits_
      }

-- | Limits settings on protection groups with arbitrary pattern type.
protectionGroupPatternTypeLimits_arbitraryPatternLimits :: Lens.Lens' ProtectionGroupPatternTypeLimits ProtectionGroupArbitraryPatternLimits
protectionGroupPatternTypeLimits_arbitraryPatternLimits = Lens.lens (\ProtectionGroupPatternTypeLimits' {arbitraryPatternLimits} -> arbitraryPatternLimits) (\s@ProtectionGroupPatternTypeLimits' {} a -> s {arbitraryPatternLimits = a} :: ProtectionGroupPatternTypeLimits)

instance
  Data.FromJSON
    ProtectionGroupPatternTypeLimits
  where
  parseJSON =
    Data.withObject
      "ProtectionGroupPatternTypeLimits"
      ( \x ->
          ProtectionGroupPatternTypeLimits'
            Prelude.<$> (x Data..: "ArbitraryPatternLimits")
      )

instance
  Prelude.Hashable
    ProtectionGroupPatternTypeLimits
  where
  hashWithSalt
    _salt
    ProtectionGroupPatternTypeLimits' {..} =
      _salt `Prelude.hashWithSalt` arbitraryPatternLimits

instance
  Prelude.NFData
    ProtectionGroupPatternTypeLimits
  where
  rnf ProtectionGroupPatternTypeLimits' {..} =
    Prelude.rnf arbitraryPatternLimits
