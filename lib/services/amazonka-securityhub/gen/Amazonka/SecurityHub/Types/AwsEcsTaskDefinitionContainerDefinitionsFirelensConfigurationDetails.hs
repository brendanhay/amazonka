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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The FireLens configuration for the container. The configuration
-- specifies and configures a log router for container logs.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails = AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails'
  { -- | The log router to use. Valid values are @fluentbit@ or @fluentd@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The options to use to configure the log router.
    --
    -- The valid option keys are as follows:
    --
    -- -   @enable-ecs-log-metadata@. The value can be @true@ or @false@.
    --
    -- -   @config-file-type@. The value can be @s3@ or @file@.
    --
    -- -   @config-file-value@. The value is either an S3 ARN or a file path.
    options :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type' - The log router to use. Valid values are @fluentbit@ or @fluentd@.
--
-- 'options', 'awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options' - The options to use to configure the log router.
--
-- The valid option keys are as follows:
--
-- -   @enable-ecs-log-metadata@. The value can be @true@ or @false@.
--
-- -   @config-file-type@. The value can be @s3@ or @file@.
--
-- -   @config-file-value@. The value is either an S3 ARN or a file path.
newAwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
newAwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails =
  AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails'
    { type' =
        Prelude.Nothing,
      options =
        Prelude.Nothing
    }

-- | The log router to use. Valid values are @fluentbit@ or @fluentd@.
awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {type'} -> type') (\s@AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {} a -> s {type' = a} :: AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails)

-- | The options to use to configure the log router.
--
-- The valid option keys are as follows:
--
-- -   @enable-ecs-log-metadata@. The value can be @true@ or @false@.
--
-- -   @config-file-type@. The value can be @s3@ or @file@.
--
-- -   @config-file-value@. The value is either an S3 ARN or a file path.
awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {options} -> options) (\s@AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {} a -> s {options = a} :: AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails'
            Prelude.<$> (x Data..:? "Type")
              Prelude.<*> (x Data..:? "Options" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` options

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {..} =
      Prelude.rnf type' `Prelude.seq` Prelude.rnf options

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Type" Data..=) Prelude.<$> type',
              ("Options" Data..=) Prelude.<$> options
            ]
        )
