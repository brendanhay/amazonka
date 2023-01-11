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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A file that contain environment variables to pass to a container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails = AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails'
  { -- | The type of environment file. The valid value is @s3@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the S3 object that contains the environment variable file.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_type' - The type of environment file. The valid value is @s3@.
--
-- 'value', 'awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_value' - The ARN of the S3 object that contains the environment variable file.
newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails =
  AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails'
    { type' =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The type of environment file. The valid value is @s3@.
awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_type :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_type = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {type'} -> type') (\s@AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {} a -> s {type' = a} :: AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails)

-- | The ARN of the S3 object that contains the environment variable file.
awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_value :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_value = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {value} -> value) (\s@AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {..} =
      Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Type" Data..=) Prelude.<$> type',
              ("Value" Data..=) Prelude.<$> value
            ]
        )
