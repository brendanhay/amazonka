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
-- Module      : Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Limits settings on protection groups with arbitrary pattern type.
--
-- /See:/ 'newProtectionGroupArbitraryPatternLimits' smart constructor.
data ProtectionGroupArbitraryPatternLimits = ProtectionGroupArbitraryPatternLimits'
  { -- | The maximum number of resources you can specify for a single arbitrary
    -- pattern in a protection group.
    maxMembers :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProtectionGroupArbitraryPatternLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxMembers', 'protectionGroupArbitraryPatternLimits_maxMembers' - The maximum number of resources you can specify for a single arbitrary
-- pattern in a protection group.
newProtectionGroupArbitraryPatternLimits ::
  -- | 'maxMembers'
  Core.Integer ->
  ProtectionGroupArbitraryPatternLimits
newProtectionGroupArbitraryPatternLimits pMaxMembers_ =
  ProtectionGroupArbitraryPatternLimits'
    { maxMembers =
        pMaxMembers_
    }

-- | The maximum number of resources you can specify for a single arbitrary
-- pattern in a protection group.
protectionGroupArbitraryPatternLimits_maxMembers :: Lens.Lens' ProtectionGroupArbitraryPatternLimits Core.Integer
protectionGroupArbitraryPatternLimits_maxMembers = Lens.lens (\ProtectionGroupArbitraryPatternLimits' {maxMembers} -> maxMembers) (\s@ProtectionGroupArbitraryPatternLimits' {} a -> s {maxMembers = a} :: ProtectionGroupArbitraryPatternLimits)

instance
  Core.FromJSON
    ProtectionGroupArbitraryPatternLimits
  where
  parseJSON =
    Core.withObject
      "ProtectionGroupArbitraryPatternLimits"
      ( \x ->
          ProtectionGroupArbitraryPatternLimits'
            Core.<$> (x Core..: "MaxMembers")
      )

instance
  Core.Hashable
    ProtectionGroupArbitraryPatternLimits

instance
  Core.NFData
    ProtectionGroupArbitraryPatternLimits
