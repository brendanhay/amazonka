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
-- Module      : Network.AWS.AppSync.Types.AwsIamConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AwsIamConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The AWS IAM configuration.
--
-- /See:/ 'newAwsIamConfig' smart constructor.
data AwsIamConfig = AwsIamConfig'
  { -- | The signing service name for AWS IAM authorization.
    signingServiceName :: Prelude.Maybe Prelude.Text,
    -- | The signing region for AWS IAM authorization.
    signingRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AwsIamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingServiceName', 'awsIamConfig_signingServiceName' - The signing service name for AWS IAM authorization.
--
-- 'signingRegion', 'awsIamConfig_signingRegion' - The signing region for AWS IAM authorization.
newAwsIamConfig ::
  AwsIamConfig
newAwsIamConfig =
  AwsIamConfig'
    { signingServiceName = Prelude.Nothing,
      signingRegion = Prelude.Nothing
    }

-- | The signing service name for AWS IAM authorization.
awsIamConfig_signingServiceName :: Lens.Lens' AwsIamConfig (Prelude.Maybe Prelude.Text)
awsIamConfig_signingServiceName = Lens.lens (\AwsIamConfig' {signingServiceName} -> signingServiceName) (\s@AwsIamConfig' {} a -> s {signingServiceName = a} :: AwsIamConfig)

-- | The signing region for AWS IAM authorization.
awsIamConfig_signingRegion :: Lens.Lens' AwsIamConfig (Prelude.Maybe Prelude.Text)
awsIamConfig_signingRegion = Lens.lens (\AwsIamConfig' {signingRegion} -> signingRegion) (\s@AwsIamConfig' {} a -> s {signingRegion = a} :: AwsIamConfig)

instance Prelude.FromJSON AwsIamConfig where
  parseJSON =
    Prelude.withObject
      "AwsIamConfig"
      ( \x ->
          AwsIamConfig'
            Prelude.<$> (x Prelude..:? "signingServiceName")
            Prelude.<*> (x Prelude..:? "signingRegion")
      )

instance Prelude.Hashable AwsIamConfig

instance Prelude.NFData AwsIamConfig

instance Prelude.ToJSON AwsIamConfig where
  toJSON AwsIamConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("signingServiceName" Prelude..=)
              Prelude.<$> signingServiceName,
            ("signingRegion" Prelude..=)
              Prelude.<$> signingRegion
          ]
      )
