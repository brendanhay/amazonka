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
-- Module      : Amazonka.SESV2.Types.DeliveryOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DeliveryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.TlsPolicy

-- | Used to associate a configuration set with a dedicated IP pool.
--
-- /See:/ 'newDeliveryOptions' smart constructor.
data DeliveryOptions = DeliveryOptions'
  { -- | Specifies whether messages that use the configuration set are required
    -- to use Transport Layer Security (TLS). If the value is @Require@,
    -- messages are only delivered if a TLS connection can be established. If
    -- the value is @Optional@, messages can be delivered in plain text if a
    -- TLS connection can\'t be established.
    tlsPolicy :: Prelude.Maybe TlsPolicy,
    -- | The name of the dedicated IP pool to associate with the configuration
    -- set.
    sendingPoolName :: Prelude.Maybe Prelude.Text
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
-- 'tlsPolicy', 'deliveryOptions_tlsPolicy' - Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
--
-- 'sendingPoolName', 'deliveryOptions_sendingPoolName' - The name of the dedicated IP pool to associate with the configuration
-- set.
newDeliveryOptions ::
  DeliveryOptions
newDeliveryOptions =
  DeliveryOptions'
    { tlsPolicy = Prelude.Nothing,
      sendingPoolName = Prelude.Nothing
    }

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
deliveryOptions_tlsPolicy :: Lens.Lens' DeliveryOptions (Prelude.Maybe TlsPolicy)
deliveryOptions_tlsPolicy = Lens.lens (\DeliveryOptions' {tlsPolicy} -> tlsPolicy) (\s@DeliveryOptions' {} a -> s {tlsPolicy = a} :: DeliveryOptions)

-- | The name of the dedicated IP pool to associate with the configuration
-- set.
deliveryOptions_sendingPoolName :: Lens.Lens' DeliveryOptions (Prelude.Maybe Prelude.Text)
deliveryOptions_sendingPoolName = Lens.lens (\DeliveryOptions' {sendingPoolName} -> sendingPoolName) (\s@DeliveryOptions' {} a -> s {sendingPoolName = a} :: DeliveryOptions)

instance Core.FromJSON DeliveryOptions where
  parseJSON =
    Core.withObject
      "DeliveryOptions"
      ( \x ->
          DeliveryOptions'
            Prelude.<$> (x Core..:? "TlsPolicy")
            Prelude.<*> (x Core..:? "SendingPoolName")
      )

instance Prelude.Hashable DeliveryOptions where
  hashWithSalt _salt DeliveryOptions' {..} =
    _salt `Prelude.hashWithSalt` tlsPolicy
      `Prelude.hashWithSalt` sendingPoolName

instance Prelude.NFData DeliveryOptions where
  rnf DeliveryOptions' {..} =
    Prelude.rnf tlsPolicy
      `Prelude.seq` Prelude.rnf sendingPoolName

instance Core.ToJSON DeliveryOptions where
  toJSON DeliveryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TlsPolicy" Core..=) Prelude.<$> tlsPolicy,
            ("SendingPoolName" Core..=)
              Prelude.<$> sendingPoolName
          ]
      )
