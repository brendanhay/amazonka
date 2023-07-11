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
-- Module      : Amazonka.Shield.Types.ProtectionGroupArbitraryPatternLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroupArbitraryPatternLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Limits settings on protection groups with arbitrary pattern type.
--
-- /See:/ 'newProtectionGroupArbitraryPatternLimits' smart constructor.
data ProtectionGroupArbitraryPatternLimits = ProtectionGroupArbitraryPatternLimits'
  { -- | The maximum number of resources you can specify for a single arbitrary
    -- pattern in a protection group.
    maxMembers :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Integer ->
  ProtectionGroupArbitraryPatternLimits
newProtectionGroupArbitraryPatternLimits pMaxMembers_ =
  ProtectionGroupArbitraryPatternLimits'
    { maxMembers =
        pMaxMembers_
    }

-- | The maximum number of resources you can specify for a single arbitrary
-- pattern in a protection group.
protectionGroupArbitraryPatternLimits_maxMembers :: Lens.Lens' ProtectionGroupArbitraryPatternLimits Prelude.Integer
protectionGroupArbitraryPatternLimits_maxMembers = Lens.lens (\ProtectionGroupArbitraryPatternLimits' {maxMembers} -> maxMembers) (\s@ProtectionGroupArbitraryPatternLimits' {} a -> s {maxMembers = a} :: ProtectionGroupArbitraryPatternLimits)

instance
  Data.FromJSON
    ProtectionGroupArbitraryPatternLimits
  where
  parseJSON =
    Data.withObject
      "ProtectionGroupArbitraryPatternLimits"
      ( \x ->
          ProtectionGroupArbitraryPatternLimits'
            Prelude.<$> (x Data..: "MaxMembers")
      )

instance
  Prelude.Hashable
    ProtectionGroupArbitraryPatternLimits
  where
  hashWithSalt
    _salt
    ProtectionGroupArbitraryPatternLimits' {..} =
      _salt `Prelude.hashWithSalt` maxMembers

instance
  Prelude.NFData
    ProtectionGroupArbitraryPatternLimits
  where
  rnf ProtectionGroupArbitraryPatternLimits' {..} =
    Prelude.rnf maxMembers
