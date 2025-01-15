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
-- Module      : Amazonka.EC2.Types.InstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMarketOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.MarketType
import Amazonka.EC2.Types.SpotMarketOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes the market (purchasing) option for the instances.
--
-- /See:/ 'newInstanceMarketOptionsRequest' smart constructor.
data InstanceMarketOptionsRequest = InstanceMarketOptionsRequest'
  { -- | The market type.
    marketType :: Prelude.Maybe MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Prelude.Maybe SpotMarketOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMarketOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marketType', 'instanceMarketOptionsRequest_marketType' - The market type.
--
-- 'spotOptions', 'instanceMarketOptionsRequest_spotOptions' - The options for Spot Instances.
newInstanceMarketOptionsRequest ::
  InstanceMarketOptionsRequest
newInstanceMarketOptionsRequest =
  InstanceMarketOptionsRequest'
    { marketType =
        Prelude.Nothing,
      spotOptions = Prelude.Nothing
    }

-- | The market type.
instanceMarketOptionsRequest_marketType :: Lens.Lens' InstanceMarketOptionsRequest (Prelude.Maybe MarketType)
instanceMarketOptionsRequest_marketType = Lens.lens (\InstanceMarketOptionsRequest' {marketType} -> marketType) (\s@InstanceMarketOptionsRequest' {} a -> s {marketType = a} :: InstanceMarketOptionsRequest)

-- | The options for Spot Instances.
instanceMarketOptionsRequest_spotOptions :: Lens.Lens' InstanceMarketOptionsRequest (Prelude.Maybe SpotMarketOptions)
instanceMarketOptionsRequest_spotOptions = Lens.lens (\InstanceMarketOptionsRequest' {spotOptions} -> spotOptions) (\s@InstanceMarketOptionsRequest' {} a -> s {spotOptions = a} :: InstanceMarketOptionsRequest)

instance
  Prelude.Hashable
    InstanceMarketOptionsRequest
  where
  hashWithSalt _salt InstanceMarketOptionsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` marketType
      `Prelude.hashWithSalt` spotOptions

instance Prelude.NFData InstanceMarketOptionsRequest where
  rnf InstanceMarketOptionsRequest' {..} =
    Prelude.rnf marketType `Prelude.seq`
      Prelude.rnf spotOptions

instance Data.ToQuery InstanceMarketOptionsRequest where
  toQuery InstanceMarketOptionsRequest' {..} =
    Prelude.mconcat
      [ "MarketType" Data.=: marketType,
        "SpotOptions" Data.=: spotOptions
      ]
