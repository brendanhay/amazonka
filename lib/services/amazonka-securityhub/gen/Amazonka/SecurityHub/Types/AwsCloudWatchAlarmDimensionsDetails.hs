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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDimensionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDimensionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the dimensions for the metric associated with the alarm.
--
-- /See:/ 'newAwsCloudWatchAlarmDimensionsDetails' smart constructor.
data AwsCloudWatchAlarmDimensionsDetails = AwsCloudWatchAlarmDimensionsDetails'
  { -- | The name of a dimension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of a dimension.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudWatchAlarmDimensionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsCloudWatchAlarmDimensionsDetails_name' - The name of a dimension.
--
-- 'value', 'awsCloudWatchAlarmDimensionsDetails_value' - The value of a dimension.
newAwsCloudWatchAlarmDimensionsDetails ::
  AwsCloudWatchAlarmDimensionsDetails
newAwsCloudWatchAlarmDimensionsDetails =
  AwsCloudWatchAlarmDimensionsDetails'
    { name =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of a dimension.
awsCloudWatchAlarmDimensionsDetails_name :: Lens.Lens' AwsCloudWatchAlarmDimensionsDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDimensionsDetails_name = Lens.lens (\AwsCloudWatchAlarmDimensionsDetails' {name} -> name) (\s@AwsCloudWatchAlarmDimensionsDetails' {} a -> s {name = a} :: AwsCloudWatchAlarmDimensionsDetails)

-- | The value of a dimension.
awsCloudWatchAlarmDimensionsDetails_value :: Lens.Lens' AwsCloudWatchAlarmDimensionsDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDimensionsDetails_value = Lens.lens (\AwsCloudWatchAlarmDimensionsDetails' {value} -> value) (\s@AwsCloudWatchAlarmDimensionsDetails' {} a -> s {value = a} :: AwsCloudWatchAlarmDimensionsDetails)

instance
  Data.FromJSON
    AwsCloudWatchAlarmDimensionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsCloudWatchAlarmDimensionsDetails"
      ( \x ->
          AwsCloudWatchAlarmDimensionsDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsCloudWatchAlarmDimensionsDetails
  where
  hashWithSalt
    _salt
    AwsCloudWatchAlarmDimensionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsCloudWatchAlarmDimensionsDetails
  where
  rnf AwsCloudWatchAlarmDimensionsDetails' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsCloudWatchAlarmDimensionsDetails
  where
  toJSON AwsCloudWatchAlarmDimensionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
