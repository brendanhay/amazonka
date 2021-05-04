{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.Types.FargatePlatformConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.FargatePlatformConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The platform configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
--
-- /See:/ 'newFargatePlatformConfiguration' smart constructor.
data FargatePlatformConfiguration = FargatePlatformConfiguration'
  { -- | The AWS Fargate platform version on which the jobs are running. A
    -- platform version is specified only for jobs running on Fargate
    -- resources. If one isn\'t specified, the @LATEST@ platform version is
    -- used by default. This will use a recent, approved version of the AWS
    -- Fargate platform for compute resources. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate platform versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FargatePlatformConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformVersion', 'fargatePlatformConfiguration_platformVersion' - The AWS Fargate platform version on which the jobs are running. A
-- platform version is specified only for jobs running on Fargate
-- resources. If one isn\'t specified, the @LATEST@ platform version is
-- used by default. This will use a recent, approved version of the AWS
-- Fargate platform for compute resources. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
newFargatePlatformConfiguration ::
  FargatePlatformConfiguration
newFargatePlatformConfiguration =
  FargatePlatformConfiguration'
    { platformVersion =
        Prelude.Nothing
    }

-- | The AWS Fargate platform version on which the jobs are running. A
-- platform version is specified only for jobs running on Fargate
-- resources. If one isn\'t specified, the @LATEST@ platform version is
-- used by default. This will use a recent, approved version of the AWS
-- Fargate platform for compute resources. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
fargatePlatformConfiguration_platformVersion :: Lens.Lens' FargatePlatformConfiguration (Prelude.Maybe Prelude.Text)
fargatePlatformConfiguration_platformVersion = Lens.lens (\FargatePlatformConfiguration' {platformVersion} -> platformVersion) (\s@FargatePlatformConfiguration' {} a -> s {platformVersion = a} :: FargatePlatformConfiguration)

instance
  Prelude.FromJSON
    FargatePlatformConfiguration
  where
  parseJSON =
    Prelude.withObject
      "FargatePlatformConfiguration"
      ( \x ->
          FargatePlatformConfiguration'
            Prelude.<$> (x Prelude..:? "platformVersion")
      )

instance
  Prelude.Hashable
    FargatePlatformConfiguration

instance Prelude.NFData FargatePlatformConfiguration

instance Prelude.ToJSON FargatePlatformConfiguration where
  toJSON FargatePlatformConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("platformVersion" Prelude..=)
              Prelude.<$> platformVersion
          ]
      )
