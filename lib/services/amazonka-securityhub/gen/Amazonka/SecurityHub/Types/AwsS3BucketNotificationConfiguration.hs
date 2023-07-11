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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail

-- | The notification configuration for the S3 bucket.
--
-- /See:/ 'newAwsS3BucketNotificationConfiguration' smart constructor.
data AwsS3BucketNotificationConfiguration = AwsS3BucketNotificationConfiguration'
  { -- | Configurations for S3 bucket notifications.
    configurations :: Prelude.Maybe [AwsS3BucketNotificationConfigurationDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'awsS3BucketNotificationConfiguration_configurations' - Configurations for S3 bucket notifications.
newAwsS3BucketNotificationConfiguration ::
  AwsS3BucketNotificationConfiguration
newAwsS3BucketNotificationConfiguration =
  AwsS3BucketNotificationConfiguration'
    { configurations =
        Prelude.Nothing
    }

-- | Configurations for S3 bucket notifications.
awsS3BucketNotificationConfiguration_configurations :: Lens.Lens' AwsS3BucketNotificationConfiguration (Prelude.Maybe [AwsS3BucketNotificationConfigurationDetail])
awsS3BucketNotificationConfiguration_configurations = Lens.lens (\AwsS3BucketNotificationConfiguration' {configurations} -> configurations) (\s@AwsS3BucketNotificationConfiguration' {} a -> s {configurations = a} :: AwsS3BucketNotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsS3BucketNotificationConfiguration
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketNotificationConfiguration"
      ( \x ->
          AwsS3BucketNotificationConfiguration'
            Prelude.<$> ( x
                            Data..:? "Configurations"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsS3BucketNotificationConfiguration
  where
  hashWithSalt
    _salt
    AwsS3BucketNotificationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` configurations

instance
  Prelude.NFData
    AwsS3BucketNotificationConfiguration
  where
  rnf AwsS3BucketNotificationConfiguration' {..} =
    Prelude.rnf configurations

instance
  Data.ToJSON
    AwsS3BucketNotificationConfiguration
  where
  toJSON AwsS3BucketNotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configurations" Data..=)
              Prelude.<$> configurations
          ]
      )
