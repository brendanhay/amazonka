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
-- Module      : Network.AWS.Shield.Types.ProtectionLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Shield.Types.Limit

-- | Limits settings on protections for your subscription.
--
-- /See:/ 'newProtectionLimits' smart constructor.
data ProtectionLimits = ProtectionLimits'
  { -- | The maximum number of resource types that you can specify in a
    -- protection.
    protectedResourceTypeLimits :: [Limit]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProtectionLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectedResourceTypeLimits', 'protectionLimits_protectedResourceTypeLimits' - The maximum number of resource types that you can specify in a
-- protection.
newProtectionLimits ::
  ProtectionLimits
newProtectionLimits =
  ProtectionLimits'
    { protectedResourceTypeLimits =
        Core.mempty
    }

-- | The maximum number of resource types that you can specify in a
-- protection.
protectionLimits_protectedResourceTypeLimits :: Lens.Lens' ProtectionLimits [Limit]
protectionLimits_protectedResourceTypeLimits = Lens.lens (\ProtectionLimits' {protectedResourceTypeLimits} -> protectedResourceTypeLimits) (\s@ProtectionLimits' {} a -> s {protectedResourceTypeLimits = a} :: ProtectionLimits) Core.. Lens._Coerce

instance Core.FromJSON ProtectionLimits where
  parseJSON =
    Core.withObject
      "ProtectionLimits"
      ( \x ->
          ProtectionLimits'
            Core.<$> ( x Core..:? "ProtectedResourceTypeLimits"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable ProtectionLimits

instance Core.NFData ProtectionLimits
