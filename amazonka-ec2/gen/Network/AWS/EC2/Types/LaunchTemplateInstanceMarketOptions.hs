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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
import Network.AWS.EC2.Types.MarketType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'newLaunchTemplateInstanceMarketOptions' smart constructor.
data LaunchTemplateInstanceMarketOptions = LaunchTemplateInstanceMarketOptions'
  { -- | The market type.
    marketType :: Prelude.Maybe MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Prelude.Maybe LaunchTemplateSpotMarketOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMarketOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marketType', 'launchTemplateInstanceMarketOptions_marketType' - The market type.
--
-- 'spotOptions', 'launchTemplateInstanceMarketOptions_spotOptions' - The options for Spot Instances.
newLaunchTemplateInstanceMarketOptions ::
  LaunchTemplateInstanceMarketOptions
newLaunchTemplateInstanceMarketOptions =
  LaunchTemplateInstanceMarketOptions'
    { marketType =
        Prelude.Nothing,
      spotOptions = Prelude.Nothing
    }

-- | The market type.
launchTemplateInstanceMarketOptions_marketType :: Lens.Lens' LaunchTemplateInstanceMarketOptions (Prelude.Maybe MarketType)
launchTemplateInstanceMarketOptions_marketType = Lens.lens (\LaunchTemplateInstanceMarketOptions' {marketType} -> marketType) (\s@LaunchTemplateInstanceMarketOptions' {} a -> s {marketType = a} :: LaunchTemplateInstanceMarketOptions)

-- | The options for Spot Instances.
launchTemplateInstanceMarketOptions_spotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptions (Prelude.Maybe LaunchTemplateSpotMarketOptions)
launchTemplateInstanceMarketOptions_spotOptions = Lens.lens (\LaunchTemplateInstanceMarketOptions' {spotOptions} -> spotOptions) (\s@LaunchTemplateInstanceMarketOptions' {} a -> s {spotOptions = a} :: LaunchTemplateInstanceMarketOptions)

instance
  Prelude.FromXML
    LaunchTemplateInstanceMarketOptions
  where
  parseXML x =
    LaunchTemplateInstanceMarketOptions'
      Prelude.<$> (x Prelude..@? "marketType")
      Prelude.<*> (x Prelude..@? "spotOptions")

instance
  Prelude.Hashable
    LaunchTemplateInstanceMarketOptions

instance
  Prelude.NFData
    LaunchTemplateInstanceMarketOptions
