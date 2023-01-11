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
-- Module      : Amazonka.DevOpsGuru.Types.CloudWatchMetricsDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudWatchMetricsDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The dimension of an Amazon CloudWatch metric that is used when DevOps
-- Guru analyzes the resources in your account for operational problems and
-- anomalous behavior. A dimension is a name\/value pair that is part of
-- the identity of a metric. A metric can have up to 10 dimensions. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Dimension Dimensions>
-- in the /Amazon CloudWatch User Guide/.
--
-- /See:/ 'newCloudWatchMetricsDimension' smart constructor.
data CloudWatchMetricsDimension = CloudWatchMetricsDimension'
  { -- | The name of the CloudWatch dimension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the CloudWatch dimension.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchMetricsDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'cloudWatchMetricsDimension_name' - The name of the CloudWatch dimension.
--
-- 'value', 'cloudWatchMetricsDimension_value' - The value of the CloudWatch dimension.
newCloudWatchMetricsDimension ::
  CloudWatchMetricsDimension
newCloudWatchMetricsDimension =
  CloudWatchMetricsDimension'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the CloudWatch dimension.
cloudWatchMetricsDimension_name :: Lens.Lens' CloudWatchMetricsDimension (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDimension_name = Lens.lens (\CloudWatchMetricsDimension' {name} -> name) (\s@CloudWatchMetricsDimension' {} a -> s {name = a} :: CloudWatchMetricsDimension)

-- | The value of the CloudWatch dimension.
cloudWatchMetricsDimension_value :: Lens.Lens' CloudWatchMetricsDimension (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDimension_value = Lens.lens (\CloudWatchMetricsDimension' {value} -> value) (\s@CloudWatchMetricsDimension' {} a -> s {value = a} :: CloudWatchMetricsDimension)

instance Data.FromJSON CloudWatchMetricsDimension where
  parseJSON =
    Data.withObject
      "CloudWatchMetricsDimension"
      ( \x ->
          CloudWatchMetricsDimension'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable CloudWatchMetricsDimension where
  hashWithSalt _salt CloudWatchMetricsDimension' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData CloudWatchMetricsDimension where
  rnf CloudWatchMetricsDimension' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
