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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A network configuration parameter to provide to the Container Network
-- Interface (CNI) plugin.
--
-- /See:/ 'newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' smart constructor.
data AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails = AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'
  { -- | The value of the property.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the property.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value' - The value of the property.
--
-- 'name', 'awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name' - The name of the property.
newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails ::
  AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails =
  AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'
    { value =
        Prelude.Nothing,
      name =
        Prelude.Nothing
    }

-- | The value of the property.
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {value} -> value) (\s@AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails)

-- | The name of the property.
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {name} -> name) (\s@AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails"
      ( \x ->
          AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Name")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails

instance
  Prelude.NFData
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails

instance
  Core.ToJSON
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
  where
  toJSON
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Value" Core..=) Prelude.<$> value,
              ("Name" Core..=) Prelude.<$> name
            ]
        )
