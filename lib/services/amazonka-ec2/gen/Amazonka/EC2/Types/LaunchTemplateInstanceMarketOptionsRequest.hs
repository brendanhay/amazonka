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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Amazonka.EC2.Types.MarketType
import qualified Amazonka.Prelude as Prelude

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'newLaunchTemplateInstanceMarketOptionsRequest' smart constructor.
data LaunchTemplateInstanceMarketOptionsRequest = LaunchTemplateInstanceMarketOptionsRequest'
  { -- | The market type.
    marketType :: Prelude.Maybe MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Prelude.Maybe LaunchTemplateSpotMarketOptionsRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      spotOptions = Prelude.Nothing
    }

-- | The market type.
launchTemplateInstanceMarketOptionsRequest_marketType :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Prelude.Maybe MarketType)
launchTemplateInstanceMarketOptionsRequest_marketType = Lens.lens (\LaunchTemplateInstanceMarketOptionsRequest' {marketType} -> marketType) (\s@LaunchTemplateInstanceMarketOptionsRequest' {} a -> s {marketType = a} :: LaunchTemplateInstanceMarketOptionsRequest)

-- | The options for Spot Instances.
launchTemplateInstanceMarketOptionsRequest_spotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Prelude.Maybe LaunchTemplateSpotMarketOptionsRequest)
launchTemplateInstanceMarketOptionsRequest_spotOptions = Lens.lens (\LaunchTemplateInstanceMarketOptionsRequest' {spotOptions} -> spotOptions) (\s@LaunchTemplateInstanceMarketOptionsRequest' {} a -> s {spotOptions = a} :: LaunchTemplateInstanceMarketOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateInstanceMarketOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceMarketOptionsRequest' {..} =
      _salt
        `Prelude.hashWithSalt` marketType
        `Prelude.hashWithSalt` spotOptions

instance
  Prelude.NFData
    LaunchTemplateInstanceMarketOptionsRequest
  where
  rnf LaunchTemplateInstanceMarketOptionsRequest' {..} =
    Prelude.rnf marketType
      `Prelude.seq` Prelude.rnf spotOptions

instance
  Data.ToQuery
    LaunchTemplateInstanceMarketOptionsRequest
  where
  toQuery
    LaunchTemplateInstanceMarketOptionsRequest' {..} =
      Prelude.mconcat
        [ "MarketType" Data.=: marketType,
          "SpotOptions" Data.=: spotOptions
        ]
