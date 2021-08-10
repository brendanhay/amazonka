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
-- Module      : Network.AWS.SESv2.Types.DeliveryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DeliveryOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.TlsPolicy

-- | Used to associate a configuration set with a dedicated IP pool.
--
-- /See:/ 'newDeliveryOptions' smart constructor.
data DeliveryOptions = DeliveryOptions'
  { -- | The name of the dedicated IP pool that you want to associate with the
    -- configuration set.
    sendingPoolName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether messages that use the configuration set are required
    -- to use Transport Layer Security (TLS). If the value is @Require@,
    -- messages are only delivered if a TLS connection can be established. If
    -- the value is @Optional@, messages can be delivered in plain text if a
    -- TLS connection can\'t be established.
    tlsPolicy :: Prelude.Maybe TlsPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeliveryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendingPoolName', 'deliveryOptions_sendingPoolName' - The name of the dedicated IP pool that you want to associate with the
-- configuration set.
--
-- 'tlsPolicy', 'deliveryOptions_tlsPolicy' - Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
newDeliveryOptions ::
  DeliveryOptions
newDeliveryOptions =
  DeliveryOptions'
    { sendingPoolName = Prelude.Nothing,
      tlsPolicy = Prelude.Nothing
    }

-- | The name of the dedicated IP pool that you want to associate with the
-- configuration set.
deliveryOptions_sendingPoolName :: Lens.Lens' DeliveryOptions (Prelude.Maybe Prelude.Text)
deliveryOptions_sendingPoolName = Lens.lens (\DeliveryOptions' {sendingPoolName} -> sendingPoolName) (\s@DeliveryOptions' {} a -> s {sendingPoolName = a} :: DeliveryOptions)

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
deliveryOptions_tlsPolicy :: Lens.Lens' DeliveryOptions (Prelude.Maybe TlsPolicy)
deliveryOptions_tlsPolicy = Lens.lens (\DeliveryOptions' {tlsPolicy} -> tlsPolicy) (\s@DeliveryOptions' {} a -> s {tlsPolicy = a} :: DeliveryOptions)

instance Core.FromJSON DeliveryOptions where
  parseJSON =
    Core.withObject
      "DeliveryOptions"
      ( \x ->
          DeliveryOptions'
            Prelude.<$> (x Core..:? "SendingPoolName")
            Prelude.<*> (x Core..:? "TlsPolicy")
      )

instance Prelude.Hashable DeliveryOptions

instance Prelude.NFData DeliveryOptions

instance Core.ToJSON DeliveryOptions where
  toJSON DeliveryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SendingPoolName" Core..=)
              Prelude.<$> sendingPoolName,
            ("TlsPolicy" Core..=) Prelude.<$> tlsPolicy
          ]
      )
