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
-- Module      : Network.AWS.Shield.Types.ProtectionLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionLimits where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.Limit

-- | Limits settings on protections for your subscription.
--
-- /See:/ 'newProtectionLimits' smart constructor.
data ProtectionLimits = ProtectionLimits'
  { -- | The maximum number of resource types that you can specify in a
    -- protection.
    protectedResourceTypeLimits :: [Limit]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.mempty
    }

-- | The maximum number of resource types that you can specify in a
-- protection.
protectionLimits_protectedResourceTypeLimits :: Lens.Lens' ProtectionLimits [Limit]
protectionLimits_protectedResourceTypeLimits = Lens.lens (\ProtectionLimits' {protectedResourceTypeLimits} -> protectedResourceTypeLimits) (\s@ProtectionLimits' {} a -> s {protectedResourceTypeLimits = a} :: ProtectionLimits) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ProtectionLimits where
  parseJSON =
    Prelude.withObject
      "ProtectionLimits"
      ( \x ->
          ProtectionLimits'
            Prelude.<$> ( x Prelude..:? "ProtectedResourceTypeLimits"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProtectionLimits

instance Prelude.NFData ProtectionLimits
