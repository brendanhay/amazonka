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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An environment variable to pass to the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails = AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails'
  { -- | The name of the environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the environment variable.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_name' - The name of the environment variable.
--
-- 'value', 'awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_value' - The value of the environment variable.
newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails =
  AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails'
    { name =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The name of the environment variable.
awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_name :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_name = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {name} -> name) (\s@AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails)

-- | The value of the environment variable.
awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_value :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_value = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {value} -> value) (\s@AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {..} =
      Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Name" Core..=) Prelude.<$> name,
              ("Value" Core..=) Prelude.<$> value
            ]
        )
