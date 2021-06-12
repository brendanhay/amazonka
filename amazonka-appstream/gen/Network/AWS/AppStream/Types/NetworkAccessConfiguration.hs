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
-- Module      : Network.AWS.AppStream.Types.NetworkAccessConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.NetworkAccessConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the network details of the fleet or image builder instance.
--
-- /See:/ 'newNetworkAccessConfiguration' smart constructor.
data NetworkAccessConfiguration = NetworkAccessConfiguration'
  { -- | The resource identifier of the elastic network interface that is
    -- attached to instances in your VPC. All network interfaces have the
    -- eni-xxxxxxxx resource identifier.
    eniId :: Core.Maybe Core.Text,
    -- | The private IP address of the elastic network interface that is attached
    -- to instances in your VPC.
    eniPrivateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkAccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eniId', 'networkAccessConfiguration_eniId' - The resource identifier of the elastic network interface that is
-- attached to instances in your VPC. All network interfaces have the
-- eni-xxxxxxxx resource identifier.
--
-- 'eniPrivateIpAddress', 'networkAccessConfiguration_eniPrivateIpAddress' - The private IP address of the elastic network interface that is attached
-- to instances in your VPC.
newNetworkAccessConfiguration ::
  NetworkAccessConfiguration
newNetworkAccessConfiguration =
  NetworkAccessConfiguration'
    { eniId = Core.Nothing,
      eniPrivateIpAddress = Core.Nothing
    }

-- | The resource identifier of the elastic network interface that is
-- attached to instances in your VPC. All network interfaces have the
-- eni-xxxxxxxx resource identifier.
networkAccessConfiguration_eniId :: Lens.Lens' NetworkAccessConfiguration (Core.Maybe Core.Text)
networkAccessConfiguration_eniId = Lens.lens (\NetworkAccessConfiguration' {eniId} -> eniId) (\s@NetworkAccessConfiguration' {} a -> s {eniId = a} :: NetworkAccessConfiguration)

-- | The private IP address of the elastic network interface that is attached
-- to instances in your VPC.
networkAccessConfiguration_eniPrivateIpAddress :: Lens.Lens' NetworkAccessConfiguration (Core.Maybe Core.Text)
networkAccessConfiguration_eniPrivateIpAddress = Lens.lens (\NetworkAccessConfiguration' {eniPrivateIpAddress} -> eniPrivateIpAddress) (\s@NetworkAccessConfiguration' {} a -> s {eniPrivateIpAddress = a} :: NetworkAccessConfiguration)

instance Core.FromJSON NetworkAccessConfiguration where
  parseJSON =
    Core.withObject
      "NetworkAccessConfiguration"
      ( \x ->
          NetworkAccessConfiguration'
            Core.<$> (x Core..:? "EniId")
            Core.<*> (x Core..:? "EniPrivateIpAddress")
      )

instance Core.Hashable NetworkAccessConfiguration

instance Core.NFData NetworkAccessConfiguration
