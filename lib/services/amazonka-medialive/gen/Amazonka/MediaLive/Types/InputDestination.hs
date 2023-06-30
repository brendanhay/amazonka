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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputDestinationVpc
import qualified Amazonka.Prelude as Prelude

-- | The settings for a PUSH type input.
--
-- /See:/ 'newInputDestination' smart constructor.
data InputDestination = InputDestination'
  { -- | The system-generated static IP address of endpoint. It remains fixed for
    -- the lifetime of the input.
    ip :: Prelude.Maybe Prelude.Text,
    -- | The port number for the input.
    port :: Prelude.Maybe Prelude.Text,
    -- | This represents the endpoint that the customer stream will be pushed to.
    url :: Prelude.Maybe Prelude.Text,
    vpc :: Prelude.Maybe InputDestinationVpc
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
    { ip = Prelude.Nothing,
      port = Prelude.Nothing,
      url = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The system-generated static IP address of endpoint. It remains fixed for
-- the lifetime of the input.
inputDestination_ip :: Lens.Lens' InputDestination (Prelude.Maybe Prelude.Text)
inputDestination_ip = Lens.lens (\InputDestination' {ip} -> ip) (\s@InputDestination' {} a -> s {ip = a} :: InputDestination)

-- | The port number for the input.
inputDestination_port :: Lens.Lens' InputDestination (Prelude.Maybe Prelude.Text)
inputDestination_port = Lens.lens (\InputDestination' {port} -> port) (\s@InputDestination' {} a -> s {port = a} :: InputDestination)

-- | This represents the endpoint that the customer stream will be pushed to.
inputDestination_url :: Lens.Lens' InputDestination (Prelude.Maybe Prelude.Text)
inputDestination_url = Lens.lens (\InputDestination' {url} -> url) (\s@InputDestination' {} a -> s {url = a} :: InputDestination)

-- | Undocumented member.
inputDestination_vpc :: Lens.Lens' InputDestination (Prelude.Maybe InputDestinationVpc)
inputDestination_vpc = Lens.lens (\InputDestination' {vpc} -> vpc) (\s@InputDestination' {} a -> s {vpc = a} :: InputDestination)

instance Data.FromJSON InputDestination where
  parseJSON =
    Data.withObject
      "InputDestination"
      ( \x ->
          InputDestination'
            Prelude.<$> (x Data..:? "ip")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "vpc")
      )

instance Prelude.Hashable InputDestination where
  hashWithSalt _salt InputDestination' {..} =
    _salt
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData InputDestination where
  rnf InputDestination' {..} =
    Prelude.rnf ip
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf vpc
