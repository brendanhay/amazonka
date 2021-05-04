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
-- Module      : Network.AWS.SES.Types.DeliveryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DeliveryOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.TlsPolicy

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS).
--
-- /See:/ 'newDeliveryOptions' smart constructor.
data DeliveryOptions = DeliveryOptions'
  { -- | Specifies whether messages that use the configuration set are required
    -- to use Transport Layer Security (TLS). If the value is @Require@,
    -- messages are only delivered if a TLS connection can be established. If
    -- the value is @Optional@, messages can be delivered in plain text if a
    -- TLS connection can\'t be established.
    tlsPolicy :: Prelude.Maybe TlsPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newDeliveryOptions ::
  DeliveryOptions
newDeliveryOptions =
  DeliveryOptions' {tlsPolicy = Prelude.Nothing}

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
deliveryOptions_tlsPolicy :: Lens.Lens' DeliveryOptions (Prelude.Maybe TlsPolicy)
deliveryOptions_tlsPolicy = Lens.lens (\DeliveryOptions' {tlsPolicy} -> tlsPolicy) (\s@DeliveryOptions' {} a -> s {tlsPolicy = a} :: DeliveryOptions)

instance Prelude.FromXML DeliveryOptions where
  parseXML x =
    DeliveryOptions'
      Prelude.<$> (x Prelude..@? "TlsPolicy")

instance Prelude.Hashable DeliveryOptions

instance Prelude.NFData DeliveryOptions

instance Prelude.ToQuery DeliveryOptions where
  toQuery DeliveryOptions' {..} =
    Prelude.mconcat ["TlsPolicy" Prelude.=: tlsPolicy]
