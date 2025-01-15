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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails

-- | Provides details about the market (purchasing) option for an Amazon EC2
-- instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails = AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails'
  { -- | The market type.
    marketType :: Prelude.Maybe Prelude.Text,
    -- | The options for Spot Instances.
    spotOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marketType', 'awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_marketType' - The market type.
--
-- 'spotOptions', 'awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_spotOptions' - The options for Spot Instances.
newAwsEc2LaunchTemplateDataInstanceMarketOptionsDetails ::
  AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
newAwsEc2LaunchTemplateDataInstanceMarketOptionsDetails =
  AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails'
    { marketType =
        Prelude.Nothing,
      spotOptions =
        Prelude.Nothing
    }

-- | The market type.
awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_marketType :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_marketType = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {marketType} -> marketType) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {} a -> s {marketType = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails)

-- | The options for Spot Instances.
awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_spotOptions :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails)
awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_spotOptions = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {spotOptions} -> spotOptions) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {} a -> s {spotOptions = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails'
            Prelude.<$> (x Data..:? "MarketType")
            Prelude.<*> (x Data..:? "SpotOptions")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` marketType
        `Prelude.hashWithSalt` spotOptions

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {..} =
      Prelude.rnf marketType `Prelude.seq`
        Prelude.rnf spotOptions

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MarketType" Data..=) Prelude.<$> marketType,
              ("SpotOptions" Data..=) Prelude.<$> spotOptions
            ]
        )
