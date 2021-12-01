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
-- Module      : Amazonka.MediaLive.Types.InputDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.InputDestinationVpc
import qualified Amazonka.Prelude as Prelude

-- | The settings for a PUSH type input.
--
-- /See:/ 'newInputDestination' smart constructor.
data InputDestination = InputDestination'
  { -- | This represents the endpoint that the customer stream will be pushed to.
    url :: Prelude.Maybe Prelude.Text,
    -- | The system-generated static IP address of endpoint. It remains fixed for
    -- the lifetime of the input.
    ip :: Prelude.Maybe Prelude.Text,
    vpc :: Prelude.Maybe InputDestinationVpc,
    -- | The port number for the input.
    port :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'inputDestination_url' - This represents the endpoint that the customer stream will be pushed to.
--
-- 'ip', 'inputDestination_ip' - The system-generated static IP address of endpoint. It remains fixed for
-- the lifetime of the input.
--
-- 'vpc', 'inputDestination_vpc' - Undocumented member.
--
-- 'port', 'inputDestination_port' - The port number for the input.
newInputDestination ::
  InputDestination
newInputDestination =
  InputDestination'
    { url = Prelude.Nothing,
      ip = Prelude.Nothing,
      vpc = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | This represents the endpoint that the customer stream will be pushed to.
inputDestination_url :: Lens.Lens' InputDestination (Prelude.Maybe Prelude.Text)
inputDestination_url = Lens.lens (\InputDestination' {url} -> url) (\s@InputDestination' {} a -> s {url = a} :: InputDestination)

-- | The system-generated static IP address of endpoint. It remains fixed for
-- the lifetime of the input.
inputDestination_ip :: Lens.Lens' InputDestination (Prelude.Maybe Prelude.Text)
inputDestination_ip = Lens.lens (\InputDestination' {ip} -> ip) (\s@InputDestination' {} a -> s {ip = a} :: InputDestination)

-- | Undocumented member.
inputDestination_vpc :: Lens.Lens' InputDestination (Prelude.Maybe InputDestinationVpc)
inputDestination_vpc = Lens.lens (\InputDestination' {vpc} -> vpc) (\s@InputDestination' {} a -> s {vpc = a} :: InputDestination)

-- | The port number for the input.
inputDestination_port :: Lens.Lens' InputDestination (Prelude.Maybe Prelude.Text)
inputDestination_port = Lens.lens (\InputDestination' {port} -> port) (\s@InputDestination' {} a -> s {port = a} :: InputDestination)

instance Core.FromJSON InputDestination where
  parseJSON =
    Core.withObject
      "InputDestination"
      ( \x ->
          InputDestination'
            Prelude.<$> (x Core..:? "url")
            Prelude.<*> (x Core..:? "ip")
            Prelude.<*> (x Core..:? "vpc")
            Prelude.<*> (x Core..:? "port")
      )

instance Prelude.Hashable InputDestination where
  hashWithSalt salt' InputDestination' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` url

instance Prelude.NFData InputDestination where
  rnf InputDestination' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf ip
