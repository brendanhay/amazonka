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
-- Module      : Network.AWS.MediaLive.Types.InputDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDestinationVpc

-- | The settings for a PUSH type input.
--
-- /See:/ 'newInputDestination' smart constructor.
data InputDestination = InputDestination'
  { -- | The system-generated static IP address of endpoint. It remains fixed for
    -- the lifetime of the input.
    ip :: Core.Maybe Core.Text,
    -- | The port number for the input.
    port :: Core.Maybe Core.Text,
    -- | This represents the endpoint that the customer stream will be pushed to.
    url :: Core.Maybe Core.Text,
    vpc :: Core.Maybe InputDestinationVpc
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'inputDestination_ip' - The system-generated static IP address of endpoint. It remains fixed for
-- the lifetime of the input.
--
-- 'port', 'inputDestination_port' - The port number for the input.
--
-- 'url', 'inputDestination_url' - This represents the endpoint that the customer stream will be pushed to.
--
-- 'vpc', 'inputDestination_vpc' - Undocumented member.
newInputDestination ::
  InputDestination
newInputDestination =
  InputDestination'
    { ip = Core.Nothing,
      port = Core.Nothing,
      url = Core.Nothing,
      vpc = Core.Nothing
    }

-- | The system-generated static IP address of endpoint. It remains fixed for
-- the lifetime of the input.
inputDestination_ip :: Lens.Lens' InputDestination (Core.Maybe Core.Text)
inputDestination_ip = Lens.lens (\InputDestination' {ip} -> ip) (\s@InputDestination' {} a -> s {ip = a} :: InputDestination)

-- | The port number for the input.
inputDestination_port :: Lens.Lens' InputDestination (Core.Maybe Core.Text)
inputDestination_port = Lens.lens (\InputDestination' {port} -> port) (\s@InputDestination' {} a -> s {port = a} :: InputDestination)

-- | This represents the endpoint that the customer stream will be pushed to.
inputDestination_url :: Lens.Lens' InputDestination (Core.Maybe Core.Text)
inputDestination_url = Lens.lens (\InputDestination' {url} -> url) (\s@InputDestination' {} a -> s {url = a} :: InputDestination)

-- | Undocumented member.
inputDestination_vpc :: Lens.Lens' InputDestination (Core.Maybe InputDestinationVpc)
inputDestination_vpc = Lens.lens (\InputDestination' {vpc} -> vpc) (\s@InputDestination' {} a -> s {vpc = a} :: InputDestination)

instance Core.FromJSON InputDestination where
  parseJSON =
    Core.withObject
      "InputDestination"
      ( \x ->
          InputDestination'
            Core.<$> (x Core..:? "ip")
            Core.<*> (x Core..:? "port")
            Core.<*> (x Core..:? "url")
            Core.<*> (x Core..:? "vpc")
      )

instance Core.Hashable InputDestination

instance Core.NFData InputDestination
