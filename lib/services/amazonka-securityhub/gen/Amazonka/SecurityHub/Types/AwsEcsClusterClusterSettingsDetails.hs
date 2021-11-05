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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether to enable CloudWatch Container Insights for the ECS
-- cluster.
--
-- /See:/ 'newAwsEcsClusterClusterSettingsDetails' smart constructor.
data AwsEcsClusterClusterSettingsDetails = AwsEcsClusterClusterSettingsDetails'
  { -- | The value of the setting.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the setting.
    name :: Prelude.Maybe Prelude.Text
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
-- 'value', 'awsEcsClusterClusterSettingsDetails_value' - The value of the setting.
--
-- 'name', 'awsEcsClusterClusterSettingsDetails_name' - The name of the setting.
newAwsEcsClusterClusterSettingsDetails ::
  AwsEcsClusterClusterSettingsDetails
newAwsEcsClusterClusterSettingsDetails =
  AwsEcsClusterClusterSettingsDetails'
    { value =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value of the setting.
awsEcsClusterClusterSettingsDetails_value :: Lens.Lens' AwsEcsClusterClusterSettingsDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterClusterSettingsDetails_value = Lens.lens (\AwsEcsClusterClusterSettingsDetails' {value} -> value) (\s@AwsEcsClusterClusterSettingsDetails' {} a -> s {value = a} :: AwsEcsClusterClusterSettingsDetails)

-- | The name of the setting.
awsEcsClusterClusterSettingsDetails_name :: Lens.Lens' AwsEcsClusterClusterSettingsDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterClusterSettingsDetails_name = Lens.lens (\AwsEcsClusterClusterSettingsDetails' {name} -> name) (\s@AwsEcsClusterClusterSettingsDetails' {} a -> s {name = a} :: AwsEcsClusterClusterSettingsDetails)

instance
  Core.FromJSON
    AwsEcsClusterClusterSettingsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsClusterClusterSettingsDetails"
      ( \x ->
          AwsEcsClusterClusterSettingsDetails'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Name")
      )

instance
  Prelude.Hashable
    AwsEcsClusterClusterSettingsDetails

instance
  Prelude.NFData
    AwsEcsClusterClusterSettingsDetails

instance
  Core.ToJSON
    AwsEcsClusterClusterSettingsDetails
  where
  toJSON AwsEcsClusterClusterSettingsDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
