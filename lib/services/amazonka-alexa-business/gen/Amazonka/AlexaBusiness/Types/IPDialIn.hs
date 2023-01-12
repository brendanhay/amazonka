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
-- Module      : Amazonka.AlexaBusiness.Types.IPDialIn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.IPDialIn where

import Amazonka.AlexaBusiness.Types.CommsProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The IP endpoint and protocol for calling.
--
-- /See:/ 'newIPDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { -- | The IP address.
    endpoint :: Prelude.Text,
    -- | The protocol, including SIP, SIPS, and H323.
    commsProtocol :: CommsProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON IPDialIn where
  parseJSON =
    Data.withObject
      "IPDialIn"
      ( \x ->
          IPDialIn'
            Prelude.<$> (x Data..: "Endpoint")
            Prelude.<*> (x Data..: "CommsProtocol")
      )

instance Prelude.Hashable IPDialIn where
  hashWithSalt _salt IPDialIn' {..} =
    _salt `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` commsProtocol

instance Prelude.NFData IPDialIn where
  rnf IPDialIn' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf commsProtocol

instance Data.ToJSON IPDialIn where
  toJSON IPDialIn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Endpoint" Data..= endpoint),
            Prelude.Just
              ("CommsProtocol" Data..= commsProtocol)
          ]
      )
