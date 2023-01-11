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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DeliveryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.TlsPolicy

-- | Used to associate a configuration set with a dedicated IP pool.
--
-- /See:/ 'newDeliveryOptions' smart constructor.
data DeliveryOptions = DeliveryOptions'
  { -- | The name of the dedicated IP pool to associate with the configuration
    -- set.
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
-- 'sendingPoolName', 'deliveryOptions_sendingPoolName' - The name of the dedicated IP pool to associate with the configuration
-- set.
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

-- | The name of the dedicated IP pool to associate with the configuration
-- set.
deliveryOptions_sendingPoolName :: Lens.Lens' DeliveryOptions (Prelude.Maybe Prelude.Text)
deliveryOptions_sendingPoolName = Lens.lens (\DeliveryOptions' {sendingPoolName} -> sendingPoolName) (\s@DeliveryOptions' {} a -> s {sendingPoolName = a} :: DeliveryOptions)

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
deliveryOptions_tlsPolicy :: Lens.Lens' DeliveryOptions (Prelude.Maybe TlsPolicy)
deliveryOptions_tlsPolicy = Lens.lens (\DeliveryOptions' {tlsPolicy} -> tlsPolicy) (\s@DeliveryOptions' {} a -> s {tlsPolicy = a} :: DeliveryOptions)

instance Data.FromJSON DeliveryOptions where
  parseJSON =
    Data.withObject
      "DeliveryOptions"
      ( \x ->
          DeliveryOptions'
            Prelude.<$> (x Data..:? "SendingPoolName")
            Prelude.<*> (x Data..:? "TlsPolicy")
      )

instance Prelude.Hashable DeliveryOptions where
  hashWithSalt _salt DeliveryOptions' {..} =
    _salt `Prelude.hashWithSalt` sendingPoolName
      `Prelude.hashWithSalt` tlsPolicy

instance Prelude.NFData DeliveryOptions where
  rnf DeliveryOptions' {..} =
    Prelude.rnf sendingPoolName
      `Prelude.seq` Prelude.rnf tlsPolicy

instance Data.ToJSON DeliveryOptions where
  toJSON DeliveryOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SendingPoolName" Data..=)
              Prelude.<$> sendingPoolName,
            ("TlsPolicy" Data..=) Prelude.<$> tlsPolicy
          ]
      )
