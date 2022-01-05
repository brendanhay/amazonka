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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the tier of the environment.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentTier' smart constructor.
data AwsElasticBeanstalkEnvironmentTier = AwsElasticBeanstalkEnvironmentTier'
  { -- | The name of the environment tier.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the environment tier.
    version :: Prelude.Maybe Prelude.Text,
    -- | The type of environment tier.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'name', 'awsElasticBeanstalkEnvironmentTier_name' - The name of the environment tier.
--
-- 'version', 'awsElasticBeanstalkEnvironmentTier_version' - The version of the environment tier.
--
-- 'type'', 'awsElasticBeanstalkEnvironmentTier_type' - The type of environment tier.
newAwsElasticBeanstalkEnvironmentTier ::
  AwsElasticBeanstalkEnvironmentTier
newAwsElasticBeanstalkEnvironmentTier =
  AwsElasticBeanstalkEnvironmentTier'
    { name =
        Prelude.Nothing,
      version = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the environment tier.
awsElasticBeanstalkEnvironmentTier_name :: Lens.Lens' AwsElasticBeanstalkEnvironmentTier (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentTier_name = Lens.lens (\AwsElasticBeanstalkEnvironmentTier' {name} -> name) (\s@AwsElasticBeanstalkEnvironmentTier' {} a -> s {name = a} :: AwsElasticBeanstalkEnvironmentTier)

-- | The version of the environment tier.
awsElasticBeanstalkEnvironmentTier_version :: Lens.Lens' AwsElasticBeanstalkEnvironmentTier (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentTier_version = Lens.lens (\AwsElasticBeanstalkEnvironmentTier' {version} -> version) (\s@AwsElasticBeanstalkEnvironmentTier' {} a -> s {version = a} :: AwsElasticBeanstalkEnvironmentTier)

-- | The type of environment tier.
awsElasticBeanstalkEnvironmentTier_type :: Lens.Lens' AwsElasticBeanstalkEnvironmentTier (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentTier_type = Lens.lens (\AwsElasticBeanstalkEnvironmentTier' {type'} -> type') (\s@AwsElasticBeanstalkEnvironmentTier' {} a -> s {type' = a} :: AwsElasticBeanstalkEnvironmentTier)

instance
  Core.FromJSON
    AwsElasticBeanstalkEnvironmentTier
  where
  parseJSON =
    Core.withObject
      "AwsElasticBeanstalkEnvironmentTier"
      ( \x ->
          AwsElasticBeanstalkEnvironmentTier'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentTier
  where
  hashWithSalt
    _salt
    AwsElasticBeanstalkEnvironmentTier' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` version
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentTier
  where
  rnf AwsElasticBeanstalkEnvironmentTier' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf type'

instance
  Core.ToJSON
    AwsElasticBeanstalkEnvironmentTier
  where
  toJSON AwsElasticBeanstalkEnvironmentTier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Version" Core..=) Prelude.<$> version,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
