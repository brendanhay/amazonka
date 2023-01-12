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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesDetails

-- | The lifecycle configuration for the objects in the S3 bucket.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationDetails = AwsS3BucketBucketLifecycleConfigurationDetails'
  { -- | The lifecycle rules.
    rules :: Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'awsS3BucketBucketLifecycleConfigurationDetails_rules' - The lifecycle rules.
newAwsS3BucketBucketLifecycleConfigurationDetails ::
  AwsS3BucketBucketLifecycleConfigurationDetails
newAwsS3BucketBucketLifecycleConfigurationDetails =
  AwsS3BucketBucketLifecycleConfigurationDetails'
    { rules =
        Prelude.Nothing
    }

-- | The lifecycle rules.
awsS3BucketBucketLifecycleConfigurationDetails_rules :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationDetails (Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesDetails])
awsS3BucketBucketLifecycleConfigurationDetails_rules = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationDetails' {rules} -> rules) (\s@AwsS3BucketBucketLifecycleConfigurationDetails' {} a -> s {rules = a} :: AwsS3BucketBucketLifecycleConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsS3BucketBucketLifecycleConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketBucketLifecycleConfigurationDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationDetails'
            Prelude.<$> (x Data..:? "Rules" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` rules

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationDetails' {..} =
      Prelude.rnf rules

instance
  Data.ToJSON
    AwsS3BucketBucketLifecycleConfigurationDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Rules" Data..=) Prelude.<$> rules]
        )
