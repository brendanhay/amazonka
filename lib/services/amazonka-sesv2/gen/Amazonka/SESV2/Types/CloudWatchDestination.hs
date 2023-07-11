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
-- Module      : Amazonka.SESV2.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.CloudWatchDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.CloudWatchDimensionConfiguration

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

instance Data.FromJSON CloudWatchDestination where
  parseJSON =
    Data.withObject
      "CloudWatchDestination"
      ( \x ->
          CloudWatchDestination'
            Prelude.<$> ( x
                            Data..:? "DimensionConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CloudWatchDestination where
  hashWithSalt _salt CloudWatchDestination' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionConfigurations

instance Prelude.NFData CloudWatchDestination where
  rnf CloudWatchDestination' {..} =
    Prelude.rnf dimensionConfigurations

instance Data.ToJSON CloudWatchDestination where
  toJSON CloudWatchDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DimensionConfigurations"
                  Data..= dimensionConfigurations
              )
          ]
      )
