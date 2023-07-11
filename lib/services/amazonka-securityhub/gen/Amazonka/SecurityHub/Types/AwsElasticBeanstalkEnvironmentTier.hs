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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the tier of the environment.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentTier' smart constructor.
data AwsElasticBeanstalkEnvironmentTier = AwsElasticBeanstalkEnvironmentTier'
  { -- | The name of the environment tier. Valid values are @WebServer@ or
    -- @Worker@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of environment tier. Valid values are @Standard@ or
    -- @SQS\/HTTP@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The version of the environment tier.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticBeanstalkEnvironmentTier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsElasticBeanstalkEnvironmentTier_name' - The name of the environment tier. Valid values are @WebServer@ or
-- @Worker@.
--
-- 'type'', 'awsElasticBeanstalkEnvironmentTier_type' - The type of environment tier. Valid values are @Standard@ or
-- @SQS\/HTTP@.
--
-- 'version', 'awsElasticBeanstalkEnvironmentTier_version' - The version of the environment tier.
newAwsElasticBeanstalkEnvironmentTier ::
  AwsElasticBeanstalkEnvironmentTier
newAwsElasticBeanstalkEnvironmentTier =
  AwsElasticBeanstalkEnvironmentTier'
    { name =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the environment tier. Valid values are @WebServer@ or
-- @Worker@.
awsElasticBeanstalkEnvironmentTier_name :: Lens.Lens' AwsElasticBeanstalkEnvironmentTier (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentTier_name = Lens.lens (\AwsElasticBeanstalkEnvironmentTier' {name} -> name) (\s@AwsElasticBeanstalkEnvironmentTier' {} a -> s {name = a} :: AwsElasticBeanstalkEnvironmentTier)

-- | The type of environment tier. Valid values are @Standard@ or
-- @SQS\/HTTP@.
awsElasticBeanstalkEnvironmentTier_type :: Lens.Lens' AwsElasticBeanstalkEnvironmentTier (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentTier_type = Lens.lens (\AwsElasticBeanstalkEnvironmentTier' {type'} -> type') (\s@AwsElasticBeanstalkEnvironmentTier' {} a -> s {type' = a} :: AwsElasticBeanstalkEnvironmentTier)

-- | The version of the environment tier.
awsElasticBeanstalkEnvironmentTier_version :: Lens.Lens' AwsElasticBeanstalkEnvironmentTier (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentTier_version = Lens.lens (\AwsElasticBeanstalkEnvironmentTier' {version} -> version) (\s@AwsElasticBeanstalkEnvironmentTier' {} a -> s {version = a} :: AwsElasticBeanstalkEnvironmentTier)

instance
  Data.FromJSON
    AwsElasticBeanstalkEnvironmentTier
  where
  parseJSON =
    Data.withObject
      "AwsElasticBeanstalkEnvironmentTier"
      ( \x ->
          AwsElasticBeanstalkEnvironmentTier'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Version")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentTier
  where
  hashWithSalt
    _salt
    AwsElasticBeanstalkEnvironmentTier' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentTier
  where
  rnf AwsElasticBeanstalkEnvironmentTier' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version

instance
  Data.ToJSON
    AwsElasticBeanstalkEnvironmentTier
  where
  toJSON AwsElasticBeanstalkEnvironmentTier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Type" Data..=) Prelude.<$> type',
            ("Version" Data..=) Prelude.<$> version
          ]
      )
