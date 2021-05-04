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
-- Module      : Network.AWS.MediaLive.Types.InputDestinationVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationVpc where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties for a VPC type input destination.
--
-- /See:/ 'newInputDestinationVpc' smart constructor.
data InputDestinationVpc = InputDestinationVpc'
  { -- | The availability zone of the Input destination.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The network interface ID of the Input destination in the VPC.
    networkInterfaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputDestinationVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'inputDestinationVpc_availabilityZone' - The availability zone of the Input destination.
--
-- 'networkInterfaceId', 'inputDestinationVpc_networkInterfaceId' - The network interface ID of the Input destination in the VPC.
newInputDestinationVpc ::
  InputDestinationVpc
newInputDestinationVpc =
  InputDestinationVpc'
    { availabilityZone =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing
    }

-- | The availability zone of the Input destination.
inputDestinationVpc_availabilityZone :: Lens.Lens' InputDestinationVpc (Prelude.Maybe Prelude.Text)
inputDestinationVpc_availabilityZone = Lens.lens (\InputDestinationVpc' {availabilityZone} -> availabilityZone) (\s@InputDestinationVpc' {} a -> s {availabilityZone = a} :: InputDestinationVpc)

-- | The network interface ID of the Input destination in the VPC.
inputDestinationVpc_networkInterfaceId :: Lens.Lens' InputDestinationVpc (Prelude.Maybe Prelude.Text)
inputDestinationVpc_networkInterfaceId = Lens.lens (\InputDestinationVpc' {networkInterfaceId} -> networkInterfaceId) (\s@InputDestinationVpc' {} a -> s {networkInterfaceId = a} :: InputDestinationVpc)

instance Prelude.FromJSON InputDestinationVpc where
  parseJSON =
    Prelude.withObject
      "InputDestinationVpc"
      ( \x ->
          InputDestinationVpc'
            Prelude.<$> (x Prelude..:? "availabilityZone")
            Prelude.<*> (x Prelude..:? "networkInterfaceId")
      )

instance Prelude.Hashable InputDestinationVpc

instance Prelude.NFData InputDestinationVpc
