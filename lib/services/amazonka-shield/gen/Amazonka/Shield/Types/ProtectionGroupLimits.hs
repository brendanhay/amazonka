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
-- Module      : Amazonka.Shield.Types.ProtectionGroupLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroupLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ProtectionGroupPatternTypeLimits

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON ProtectionGroupLimits where
  parseJSON =
    Data.withObject
      "ProtectionGroupLimits"
      ( \x ->
          ProtectionGroupLimits'
            Prelude.<$> (x Data..: "MaxProtectionGroups")
            Prelude.<*> (x Data..: "PatternTypeLimits")
      )

instance Prelude.Hashable ProtectionGroupLimits where
  hashWithSalt _salt ProtectionGroupLimits' {..} =
    _salt
      `Prelude.hashWithSalt` maxProtectionGroups
      `Prelude.hashWithSalt` patternTypeLimits

instance Prelude.NFData ProtectionGroupLimits where
  rnf ProtectionGroupLimits' {..} =
    Prelude.rnf maxProtectionGroups `Prelude.seq`
      Prelude.rnf patternTypeLimits
