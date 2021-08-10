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
-- Module      : Network.AWS.CertificateManager.Types.ExpiryEventsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ExpiryEventsConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Object containing expiration events options associated with an AWS
-- account.
--
-- /See:/ 'newExpiryEventsConfiguration' smart constructor.
data ExpiryEventsConfiguration = ExpiryEventsConfiguration'
  { -- | Specifies the number of days prior to certificate expiration when ACM
    -- starts generating @EventBridge@ events. ACM sends one event per day per
    -- certificate until the certificate expires. By default, accounts receive
    -- events starting 45 days before certificate expiration.
    daysBeforeExpiry :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpiryEventsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'daysBeforeExpiry', 'expiryEventsConfiguration_daysBeforeExpiry' - Specifies the number of days prior to certificate expiration when ACM
-- starts generating @EventBridge@ events. ACM sends one event per day per
-- certificate until the certificate expires. By default, accounts receive
-- events starting 45 days before certificate expiration.
newExpiryEventsConfiguration ::
  ExpiryEventsConfiguration
newExpiryEventsConfiguration =
  ExpiryEventsConfiguration'
    { daysBeforeExpiry =
        Prelude.Nothing
    }

-- | Specifies the number of days prior to certificate expiration when ACM
-- starts generating @EventBridge@ events. ACM sends one event per day per
-- certificate until the certificate expires. By default, accounts receive
-- events starting 45 days before certificate expiration.
expiryEventsConfiguration_daysBeforeExpiry :: Lens.Lens' ExpiryEventsConfiguration (Prelude.Maybe Prelude.Natural)
expiryEventsConfiguration_daysBeforeExpiry = Lens.lens (\ExpiryEventsConfiguration' {daysBeforeExpiry} -> daysBeforeExpiry) (\s@ExpiryEventsConfiguration' {} a -> s {daysBeforeExpiry = a} :: ExpiryEventsConfiguration)

instance Core.FromJSON ExpiryEventsConfiguration where
  parseJSON =
    Core.withObject
      "ExpiryEventsConfiguration"
      ( \x ->
          ExpiryEventsConfiguration'
            Prelude.<$> (x Core..:? "DaysBeforeExpiry")
      )

instance Prelude.Hashable ExpiryEventsConfiguration

instance Prelude.NFData ExpiryEventsConfiguration

instance Core.ToJSON ExpiryEventsConfiguration where
  toJSON ExpiryEventsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DaysBeforeExpiry" Core..=)
              Prelude.<$> daysBeforeExpiry
          ]
      )
