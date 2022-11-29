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
-- Module      : Amazonka.Batch.Types.FargatePlatformConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.FargatePlatformConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The platform configuration for jobs that are running on Fargate
-- resources. Jobs that run on EC2 resources must not specify this
-- parameter.
--
-- /See:/ 'newFargatePlatformConfiguration' smart constructor.
data FargatePlatformConfiguration = FargatePlatformConfiguration'
  { -- | The Fargate platform version where the jobs are running. A platform
    -- version is specified only for jobs that are running on Fargate
    -- resources. If one isn\'t specified, the @LATEST@ platform version is
    -- used by default. This uses a recent, approved version of the Fargate
    -- platform for compute resources. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FargatePlatformConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformVersion', 'fargatePlatformConfiguration_platformVersion' - The Fargate platform version where the jobs are running. A platform
-- version is specified only for jobs that are running on Fargate
-- resources. If one isn\'t specified, the @LATEST@ platform version is
-- used by default. This uses a recent, approved version of the Fargate
-- platform for compute resources. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
newFargatePlatformConfiguration ::
  FargatePlatformConfiguration
newFargatePlatformConfiguration =
  FargatePlatformConfiguration'
    { platformVersion =
        Prelude.Nothing
    }

-- | The Fargate platform version where the jobs are running. A platform
-- version is specified only for jobs that are running on Fargate
-- resources. If one isn\'t specified, the @LATEST@ platform version is
-- used by default. This uses a recent, approved version of the Fargate
-- platform for compute resources. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
fargatePlatformConfiguration_platformVersion :: Lens.Lens' FargatePlatformConfiguration (Prelude.Maybe Prelude.Text)
fargatePlatformConfiguration_platformVersion = Lens.lens (\FargatePlatformConfiguration' {platformVersion} -> platformVersion) (\s@FargatePlatformConfiguration' {} a -> s {platformVersion = a} :: FargatePlatformConfiguration)

instance Core.FromJSON FargatePlatformConfiguration where
  parseJSON =
    Core.withObject
      "FargatePlatformConfiguration"
      ( \x ->
          FargatePlatformConfiguration'
            Prelude.<$> (x Core..:? "platformVersion")
      )

instance
  Prelude.Hashable
    FargatePlatformConfiguration
  where
  hashWithSalt _salt FargatePlatformConfiguration' {..} =
    _salt `Prelude.hashWithSalt` platformVersion

instance Prelude.NFData FargatePlatformConfiguration where
  rnf FargatePlatformConfiguration' {..} =
    Prelude.rnf platformVersion

instance Core.ToJSON FargatePlatformConfiguration where
  toJSON FargatePlatformConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("platformVersion" Core..=)
              Prelude.<$> platformVersion
          ]
      )
