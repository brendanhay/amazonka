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
-- Module      : Network.AWS.PinpointEmail.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.PinpointEmail.Types.CloudWatchDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.PinpointEmail.Types.CloudWatchDimensionConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | An object that defines an Amazon CloudWatch destination for email
-- events. You can use Amazon CloudWatch to monitor and gain insights on
-- your email sending metrics.
--
-- /See:/ 'newCloudWatchDestination' smart constructor.
data CloudWatchDestination = CloudWatchDestination'
  { -- | An array of objects that define the dimensions to use when you send
    -- email events to Amazon CloudWatch.
    dimensionConfigurations :: [CloudWatchDimensionConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionConfigurations', 'cloudWatchDestination_dimensionConfigurations' - An array of objects that define the dimensions to use when you send
-- email events to Amazon CloudWatch.
newCloudWatchDestination ::
  CloudWatchDestination
newCloudWatchDestination =
  CloudWatchDestination'
    { dimensionConfigurations =
        Prelude.mempty
    }

-- | An array of objects that define the dimensions to use when you send
-- email events to Amazon CloudWatch.
cloudWatchDestination_dimensionConfigurations :: Lens.Lens' CloudWatchDestination [CloudWatchDimensionConfiguration]
cloudWatchDestination_dimensionConfigurations = Lens.lens (\CloudWatchDestination' {dimensionConfigurations} -> dimensionConfigurations) (\s@CloudWatchDestination' {} a -> s {dimensionConfigurations = a} :: CloudWatchDestination) Prelude.. Lens.coerced

instance Core.FromJSON CloudWatchDestination where
  parseJSON =
    Core.withObject
      "CloudWatchDestination"
      ( \x ->
          CloudWatchDestination'
            Prelude.<$> ( x Core..:? "DimensionConfigurations"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CloudWatchDestination

instance Prelude.NFData CloudWatchDestination

instance Core.ToJSON CloudWatchDestination where
  toJSON CloudWatchDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DimensionConfigurations"
                  Core..= dimensionConfigurations
              )
          ]
      )
