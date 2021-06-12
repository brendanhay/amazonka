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
-- Module      : Network.AWS.AlexaBusiness.Types.IPDialIn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.IPDialIn where

import Network.AWS.AlexaBusiness.Types.CommsProtocol
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The IP endpoint and protocol for calling.
--
-- /See:/ 'newIPDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { -- | The IP address.
    endpoint :: Core.Text,
    -- | The protocol, including SIP, SIPS, and H323.
    commsProtocol :: CommsProtocol
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IPDialIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'iPDialIn_endpoint' - The IP address.
--
-- 'commsProtocol', 'iPDialIn_commsProtocol' - The protocol, including SIP, SIPS, and H323.
newIPDialIn ::
  -- | 'endpoint'
  Core.Text ->
  -- | 'commsProtocol'
  CommsProtocol ->
  IPDialIn
newIPDialIn pEndpoint_ pCommsProtocol_ =
  IPDialIn'
    { endpoint = pEndpoint_,
      commsProtocol = pCommsProtocol_
    }

-- | The IP address.
iPDialIn_endpoint :: Lens.Lens' IPDialIn Core.Text
iPDialIn_endpoint = Lens.lens (\IPDialIn' {endpoint} -> endpoint) (\s@IPDialIn' {} a -> s {endpoint = a} :: IPDialIn)

-- | The protocol, including SIP, SIPS, and H323.
iPDialIn_commsProtocol :: Lens.Lens' IPDialIn CommsProtocol
iPDialIn_commsProtocol = Lens.lens (\IPDialIn' {commsProtocol} -> commsProtocol) (\s@IPDialIn' {} a -> s {commsProtocol = a} :: IPDialIn)

instance Core.FromJSON IPDialIn where
  parseJSON =
    Core.withObject
      "IPDialIn"
      ( \x ->
          IPDialIn'
            Core.<$> (x Core..: "Endpoint")
            Core.<*> (x Core..: "CommsProtocol")
      )

instance Core.Hashable IPDialIn

instance Core.NFData IPDialIn

instance Core.ToJSON IPDialIn where
  toJSON IPDialIn' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Endpoint" Core..= endpoint),
            Core.Just ("CommsProtocol" Core..= commsProtocol)
          ]
      )
