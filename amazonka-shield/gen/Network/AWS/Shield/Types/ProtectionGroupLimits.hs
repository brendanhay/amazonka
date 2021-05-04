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
-- Module      : Network.AWS.Shield.Types.ProtectionGroupLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupLimits where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits

-- | Limits settings on protection groups for your subscription.
--
-- /See:/ 'newProtectionGroupLimits' smart constructor.
data ProtectionGroupLimits = ProtectionGroupLimits'
  { -- | The maximum number of protection groups that you can have at one time.
    maxProtectionGroups :: Prelude.Integer,
    -- | Limits settings by pattern type in the protection groups for your
    -- subscription.
    patternTypeLimits :: ProtectionGroupPatternTypeLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProtectionGroupLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxProtectionGroups', 'protectionGroupLimits_maxProtectionGroups' - The maximum number of protection groups that you can have at one time.
--
-- 'patternTypeLimits', 'protectionGroupLimits_patternTypeLimits' - Limits settings by pattern type in the protection groups for your
-- subscription.
newProtectionGroupLimits ::
  -- | 'maxProtectionGroups'
  Prelude.Integer ->
  -- | 'patternTypeLimits'
  ProtectionGroupPatternTypeLimits ->
  ProtectionGroupLimits
newProtectionGroupLimits
  pMaxProtectionGroups_
  pPatternTypeLimits_ =
    ProtectionGroupLimits'
      { maxProtectionGroups =
          pMaxProtectionGroups_,
        patternTypeLimits = pPatternTypeLimits_
      }

-- | The maximum number of protection groups that you can have at one time.
protectionGroupLimits_maxProtectionGroups :: Lens.Lens' ProtectionGroupLimits Prelude.Integer
protectionGroupLimits_maxProtectionGroups = Lens.lens (\ProtectionGroupLimits' {maxProtectionGroups} -> maxProtectionGroups) (\s@ProtectionGroupLimits' {} a -> s {maxProtectionGroups = a} :: ProtectionGroupLimits)

-- | Limits settings by pattern type in the protection groups for your
-- subscription.
protectionGroupLimits_patternTypeLimits :: Lens.Lens' ProtectionGroupLimits ProtectionGroupPatternTypeLimits
protectionGroupLimits_patternTypeLimits = Lens.lens (\ProtectionGroupLimits' {patternTypeLimits} -> patternTypeLimits) (\s@ProtectionGroupLimits' {} a -> s {patternTypeLimits = a} :: ProtectionGroupLimits)

instance Prelude.FromJSON ProtectionGroupLimits where
  parseJSON =
    Prelude.withObject
      "ProtectionGroupLimits"
      ( \x ->
          ProtectionGroupLimits'
            Prelude.<$> (x Prelude..: "MaxProtectionGroups")
            Prelude.<*> (x Prelude..: "PatternTypeLimits")
      )

instance Prelude.Hashable ProtectionGroupLimits

instance Prelude.NFData ProtectionGroupLimits
