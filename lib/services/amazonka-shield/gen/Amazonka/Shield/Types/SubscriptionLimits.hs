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
-- Module      : Amazonka.Shield.Types.SubscriptionLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.SubscriptionLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ProtectionGroupLimits
import Amazonka.Shield.Types.ProtectionLimits

-- | Limits settings for your subscription.
--
-- /See:/ 'newSubscriptionLimits' smart constructor.
data SubscriptionLimits = SubscriptionLimits'
  { -- | Limits settings on protections for your subscription.
    protectionLimits :: ProtectionLimits,
    -- | Limits settings on protection groups for your subscription.
    protectionGroupLimits :: ProtectionGroupLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON SubscriptionLimits where
  parseJSON =
    Data.withObject
      "SubscriptionLimits"
      ( \x ->
          SubscriptionLimits'
            Prelude.<$> (x Data..: "ProtectionLimits")
            Prelude.<*> (x Data..: "ProtectionGroupLimits")
      )

instance Prelude.Hashable SubscriptionLimits where
  hashWithSalt _salt SubscriptionLimits' {..} =
    _salt `Prelude.hashWithSalt` protectionLimits
      `Prelude.hashWithSalt` protectionGroupLimits

instance Prelude.NFData SubscriptionLimits where
  rnf SubscriptionLimits' {..} =
    Prelude.rnf protectionLimits
      `Prelude.seq` Prelude.rnf protectionGroupLimits
