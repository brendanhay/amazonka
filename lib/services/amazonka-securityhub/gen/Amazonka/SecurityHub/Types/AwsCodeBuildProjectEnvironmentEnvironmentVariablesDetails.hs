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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an environment variable that is available to builds
-- for the build project.
--
-- /See:/ 'newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' smart constructor.
data AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails = AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'
  { -- | The name of the environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of environment variable.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The value of the environment variable.
    value :: Prelude.Maybe Prelude.Text
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
-- 'name', 'awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name' - The name of the environment variable.
--
-- 'type'', 'awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type' - The type of environment variable.
--
-- 'value', 'awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value' - The value of the environment variable.
newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails ::
  AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails =
  AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'
    { name =
        Prelude.Nothing,
      type' =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The name of the environment variable.
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name :: Lens.Lens' AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name = Lens.lens (\AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {name} -> name) (\s@AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {} a -> s {name = a} :: AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails)

-- | The type of environment variable.
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type :: Lens.Lens' AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type = Lens.lens (\AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {type'} -> type') (\s@AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {} a -> s {type' = a} :: AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails)

-- | The value of the environment variable.
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value :: Lens.Lens' AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value = Lens.lens (\AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {value} -> value) (\s@AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {} a -> s {value = a} :: AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails)

instance
  Data.FromJSON
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
  where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails"
      ( \x ->
          AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Type")
              Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
  where
  rnf
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf type'
        `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
  where
  toJSON
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              ("Type" Data..=) Prelude.<$> type',
              ("Value" Data..=) Prelude.<$> value
            ]
        )
