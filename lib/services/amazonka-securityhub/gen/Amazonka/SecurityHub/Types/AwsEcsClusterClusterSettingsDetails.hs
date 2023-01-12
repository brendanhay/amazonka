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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether to enable CloudWatch Container Insights for the ECS
-- cluster.
--
-- /See:/ 'newAwsEcsClusterClusterSettingsDetails' smart constructor.
data AwsEcsClusterClusterSettingsDetails = AwsEcsClusterClusterSettingsDetails'
  { -- | The name of the setting. The valid value is @containerInsights@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the setting. Valid values are @disabled@ or @enabled@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsClusterClusterSettingsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsEcsClusterClusterSettingsDetails_name' - The name of the setting. The valid value is @containerInsights@.
--
-- 'value', 'awsEcsClusterClusterSettingsDetails_value' - The value of the setting. Valid values are @disabled@ or @enabled@.
newAwsEcsClusterClusterSettingsDetails ::
  AwsEcsClusterClusterSettingsDetails
newAwsEcsClusterClusterSettingsDetails =
  AwsEcsClusterClusterSettingsDetails'
    { name =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the setting. The valid value is @containerInsights@.
awsEcsClusterClusterSettingsDetails_name :: Lens.Lens' AwsEcsClusterClusterSettingsDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterClusterSettingsDetails_name = Lens.lens (\AwsEcsClusterClusterSettingsDetails' {name} -> name) (\s@AwsEcsClusterClusterSettingsDetails' {} a -> s {name = a} :: AwsEcsClusterClusterSettingsDetails)

-- | The value of the setting. Valid values are @disabled@ or @enabled@.
awsEcsClusterClusterSettingsDetails_value :: Lens.Lens' AwsEcsClusterClusterSettingsDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterClusterSettingsDetails_value = Lens.lens (\AwsEcsClusterClusterSettingsDetails' {value} -> value) (\s@AwsEcsClusterClusterSettingsDetails' {} a -> s {value = a} :: AwsEcsClusterClusterSettingsDetails)

instance
  Data.FromJSON
    AwsEcsClusterClusterSettingsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsClusterClusterSettingsDetails"
      ( \x ->
          AwsEcsClusterClusterSettingsDetails'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsEcsClusterClusterSettingsDetails
  where
  hashWithSalt
    _salt
    AwsEcsClusterClusterSettingsDetails' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsEcsClusterClusterSettingsDetails
  where
  rnf AwsEcsClusterClusterSettingsDetails' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsEcsClusterClusterSettingsDetails
  where
  toJSON AwsEcsClusterClusterSettingsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
