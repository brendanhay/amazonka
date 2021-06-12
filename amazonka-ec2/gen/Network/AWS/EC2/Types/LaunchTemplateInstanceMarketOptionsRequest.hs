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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Network.AWS.EC2.Types.MarketType
import qualified Network.AWS.Lens as Lens

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'newLaunchTemplateInstanceMarketOptionsRequest' smart constructor.
data LaunchTemplateInstanceMarketOptionsRequest = LaunchTemplateInstanceMarketOptionsRequest'
  { -- | The market type.
    marketType :: Core.Maybe MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Core.Maybe LaunchTemplateSpotMarketOptionsRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMarketOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marketType', 'launchTemplateInstanceMarketOptionsRequest_marketType' - The market type.
--
-- 'spotOptions', 'launchTemplateInstanceMarketOptionsRequest_spotOptions' - The options for Spot Instances.
newLaunchTemplateInstanceMarketOptionsRequest ::
  LaunchTemplateInstanceMarketOptionsRequest
newLaunchTemplateInstanceMarketOptionsRequest =
  LaunchTemplateInstanceMarketOptionsRequest'
    { marketType =
        Core.Nothing,
      spotOptions = Core.Nothing
    }

-- | The market type.
launchTemplateInstanceMarketOptionsRequest_marketType :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Core.Maybe MarketType)
launchTemplateInstanceMarketOptionsRequest_marketType = Lens.lens (\LaunchTemplateInstanceMarketOptionsRequest' {marketType} -> marketType) (\s@LaunchTemplateInstanceMarketOptionsRequest' {} a -> s {marketType = a} :: LaunchTemplateInstanceMarketOptionsRequest)

-- | The options for Spot Instances.
launchTemplateInstanceMarketOptionsRequest_spotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Core.Maybe LaunchTemplateSpotMarketOptionsRequest)
launchTemplateInstanceMarketOptionsRequest_spotOptions = Lens.lens (\LaunchTemplateInstanceMarketOptionsRequest' {spotOptions} -> spotOptions) (\s@LaunchTemplateInstanceMarketOptionsRequest' {} a -> s {spotOptions = a} :: LaunchTemplateInstanceMarketOptionsRequest)

instance
  Core.Hashable
    LaunchTemplateInstanceMarketOptionsRequest

instance
  Core.NFData
    LaunchTemplateInstanceMarketOptionsRequest

instance
  Core.ToQuery
    LaunchTemplateInstanceMarketOptionsRequest
  where
  toQuery
    LaunchTemplateInstanceMarketOptionsRequest' {..} =
      Core.mconcat
        [ "MarketType" Core.=: marketType,
          "SpotOptions" Core.=: spotOptions
        ]
