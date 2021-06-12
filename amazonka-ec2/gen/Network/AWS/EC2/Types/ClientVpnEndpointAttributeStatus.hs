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
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatusCode
import qualified Network.AWS.Lens as Lens

-- | Describes the status of the Client VPN endpoint attribute.
--
-- /See:/ 'newClientVpnEndpointAttributeStatus' smart constructor.
data ClientVpnEndpointAttributeStatus = ClientVpnEndpointAttributeStatus'
  { -- | The status message.
    message :: Core.Maybe Core.Text,
    -- | The status code.
    code :: Core.Maybe ClientVpnEndpointAttributeStatusCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientVpnEndpointAttributeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'clientVpnEndpointAttributeStatus_message' - The status message.
--
-- 'code', 'clientVpnEndpointAttributeStatus_code' - The status code.
newClientVpnEndpointAttributeStatus ::
  ClientVpnEndpointAttributeStatus
newClientVpnEndpointAttributeStatus =
  ClientVpnEndpointAttributeStatus'
    { message =
        Core.Nothing,
      code = Core.Nothing
    }

-- | The status message.
clientVpnEndpointAttributeStatus_message :: Lens.Lens' ClientVpnEndpointAttributeStatus (Core.Maybe Core.Text)
clientVpnEndpointAttributeStatus_message = Lens.lens (\ClientVpnEndpointAttributeStatus' {message} -> message) (\s@ClientVpnEndpointAttributeStatus' {} a -> s {message = a} :: ClientVpnEndpointAttributeStatus)

-- | The status code.
clientVpnEndpointAttributeStatus_code :: Lens.Lens' ClientVpnEndpointAttributeStatus (Core.Maybe ClientVpnEndpointAttributeStatusCode)
clientVpnEndpointAttributeStatus_code = Lens.lens (\ClientVpnEndpointAttributeStatus' {code} -> code) (\s@ClientVpnEndpointAttributeStatus' {} a -> s {code = a} :: ClientVpnEndpointAttributeStatus)

instance
  Core.FromXML
    ClientVpnEndpointAttributeStatus
  where
  parseXML x =
    ClientVpnEndpointAttributeStatus'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance
  Core.Hashable
    ClientVpnEndpointAttributeStatus

instance Core.NFData ClientVpnEndpointAttributeStatus
