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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an environment variable that is available to builds
-- for the build project.
--
-- /See:/ 'newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' smart constructor.
data AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails = AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'
  { -- | The value of the environment variable.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of environment variable.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value' - The value of the environment variable.
--
-- 'name', 'awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name' - The name of the environment variable.
--
-- 'type'', 'awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type' - The type of environment variable.
newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails ::
  AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails =
  AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'
    { value =
        Prelude.Nothing,
      name =
        Prelude.Nothing,
      type' =
        Prelude.Nothing
    }

-- | The value of the environment variable.
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value :: Lens.Lens' AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value = Lens.lens (\AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {value} -> value) (\s@AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {} a -> s {value = a} :: AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails)

-- | The name of the environment variable.
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name :: Lens.Lens' AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name = Lens.lens (\AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {name} -> name) (\s@AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {} a -> s {name = a} :: AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails)

-- | The type of environment variable.
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type :: Lens.Lens' AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type = Lens.lens (\AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {type'} -> type') (\s@AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {} a -> s {type' = a} :: AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails)

instance
  Core.FromJSON
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
  where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails"
      ( \x ->
          AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Name")
              Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails

instance
  Prelude.NFData
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails

instance
  Core.ToJSON
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
  where
  toJSON
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Value" Core..=) Prelude.<$> value,
              ("Name" Core..=) Prelude.<$> name,
              ("Type" Core..=) Prelude.<$> type'
            ]
        )
