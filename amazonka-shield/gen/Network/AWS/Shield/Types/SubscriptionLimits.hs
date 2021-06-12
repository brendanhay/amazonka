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
-- Module      : Network.AWS.Shield.Types.SubscriptionLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubscriptionLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Shield.Types.ProtectionGroupLimits
import Network.AWS.Shield.Types.ProtectionLimits

-- | Limits settings for your subscription.
--
-- /See:/ 'newSubscriptionLimits' smart constructor.
data SubscriptionLimits = SubscriptionLimits'
  { -- | Limits settings on protections for your subscription.
    protectionLimits :: ProtectionLimits,
    -- | Limits settings on protection groups for your subscription.
    protectionGroupLimits :: ProtectionGroupLimits
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubscriptionLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionLimits', 'subscriptionLimits_protectionLimits' - Limits settings on protections for your subscription.
--
-- 'protectionGroupLimits', 'subscriptionLimits_protectionGroupLimits' - Limits settings on protection groups for your subscription.
newSubscriptionLimits ::
  -- | 'protectionLimits'
  ProtectionLimits ->
  -- | 'protectionGroupLimits'
  ProtectionGroupLimits ->
  SubscriptionLimits
newSubscriptionLimits
  pProtectionLimits_
  pProtectionGroupLimits_ =
    SubscriptionLimits'
      { protectionLimits =
          pProtectionLimits_,
        protectionGroupLimits = pProtectionGroupLimits_
      }

-- | Limits settings on protections for your subscription.
subscriptionLimits_protectionLimits :: Lens.Lens' SubscriptionLimits ProtectionLimits
subscriptionLimits_protectionLimits = Lens.lens (\SubscriptionLimits' {protectionLimits} -> protectionLimits) (\s@SubscriptionLimits' {} a -> s {protectionLimits = a} :: SubscriptionLimits)

-- | Limits settings on protection groups for your subscription.
subscriptionLimits_protectionGroupLimits :: Lens.Lens' SubscriptionLimits ProtectionGroupLimits
subscriptionLimits_protectionGroupLimits = Lens.lens (\SubscriptionLimits' {protectionGroupLimits} -> protectionGroupLimits) (\s@SubscriptionLimits' {} a -> s {protectionGroupLimits = a} :: SubscriptionLimits)

instance Core.FromJSON SubscriptionLimits where
  parseJSON =
    Core.withObject
      "SubscriptionLimits"
      ( \x ->
          SubscriptionLimits'
            Core.<$> (x Core..: "ProtectionLimits")
            Core.<*> (x Core..: "ProtectionGroupLimits")
      )

instance Core.Hashable SubscriptionLimits

instance Core.NFData SubscriptionLimits
