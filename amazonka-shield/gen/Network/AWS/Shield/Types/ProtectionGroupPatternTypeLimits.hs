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
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits

-- | Limits settings by pattern type in the protection groups for your
-- subscription.
--
-- /See:/ 'newProtectionGroupPatternTypeLimits' smart constructor.
data ProtectionGroupPatternTypeLimits = ProtectionGroupPatternTypeLimits'
  { -- | Limits settings on protection groups with arbitrary pattern type.
    arbitraryPatternLimits :: ProtectionGroupArbitraryPatternLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    ProtectionGroupPatternTypeLimits
  where
  parseJSON =
    Prelude.withObject
      "ProtectionGroupPatternTypeLimits"
      ( \x ->
          ProtectionGroupPatternTypeLimits'
            Prelude.<$> (x Prelude..: "ArbitraryPatternLimits")
      )

instance
  Prelude.Hashable
    ProtectionGroupPatternTypeLimits

instance
  Prelude.NFData
    ProtectionGroupPatternTypeLimits
