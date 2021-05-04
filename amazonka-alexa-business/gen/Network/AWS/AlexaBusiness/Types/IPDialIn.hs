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
-- Module      : Network.AWS.AlexaBusiness.Types.IPDialIn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.IPDialIn where

import Network.AWS.AlexaBusiness.Types.CommsProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The IP endpoint and protocol for calling.
--
-- /See:/ 'newIPDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { -- | The IP address.
    endpoint :: Prelude.Text,
    -- | The protocol, including SIP, SIPS, and H323.
    commsProtocol :: CommsProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'commsProtocol'
  CommsProtocol ->
  IPDialIn
newIPDialIn pEndpoint_ pCommsProtocol_ =
  IPDialIn'
    { endpoint = pEndpoint_,
      commsProtocol = pCommsProtocol_
    }

-- | The IP address.
iPDialIn_endpoint :: Lens.Lens' IPDialIn Prelude.Text
iPDialIn_endpoint = Lens.lens (\IPDialIn' {endpoint} -> endpoint) (\s@IPDialIn' {} a -> s {endpoint = a} :: IPDialIn)

-- | The protocol, including SIP, SIPS, and H323.
iPDialIn_commsProtocol :: Lens.Lens' IPDialIn CommsProtocol
iPDialIn_commsProtocol = Lens.lens (\IPDialIn' {commsProtocol} -> commsProtocol) (\s@IPDialIn' {} a -> s {commsProtocol = a} :: IPDialIn)

instance Prelude.FromJSON IPDialIn where
  parseJSON =
    Prelude.withObject
      "IPDialIn"
      ( \x ->
          IPDialIn'
            Prelude.<$> (x Prelude..: "Endpoint")
            Prelude.<*> (x Prelude..: "CommsProtocol")
      )

instance Prelude.Hashable IPDialIn

instance Prelude.NFData IPDialIn

instance Prelude.ToJSON IPDialIn where
  toJSON IPDialIn' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Endpoint" Prelude..= endpoint),
            Prelude.Just
              ("CommsProtocol" Prelude..= commsProtocol)
          ]
      )
