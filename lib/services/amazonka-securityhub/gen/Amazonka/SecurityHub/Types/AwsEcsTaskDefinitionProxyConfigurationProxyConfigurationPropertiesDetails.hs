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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A network configuration parameter to provide to the Container Network
-- Interface (CNI) plugin.
--
-- /See:/ 'newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' smart constructor.
data AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails = AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'
  { -- | The name of the property.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the property.
    value :: Prelude.Maybe Prelude.Text
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
-- 'name', 'awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name' - The name of the property.
--
-- 'value', 'awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value' - The value of the property.
newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails ::
  AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails =
  AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'
    { name =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The name of the property.
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {name} -> name) (\s@AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails)

-- | The value of the property.
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {value} -> value) (\s@AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails"
      ( \x ->
          AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
  where
  rnf
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {..} =
      Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
  where
  toJSON
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              ("Value" Data..=) Prelude.<$> value
            ]
        )
