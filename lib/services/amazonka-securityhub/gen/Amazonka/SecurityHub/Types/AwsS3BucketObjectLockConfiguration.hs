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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDetails

-- | The container element for S3 Object Lock configuration parameters. In
-- Amazon S3, Object Lock can help prevent objects from being deleted or
-- overwritten for a fixed amount of time or indefinitely.
--
-- /See:/ 'newAwsS3BucketObjectLockConfiguration' smart constructor.
data AwsS3BucketObjectLockConfiguration = AwsS3BucketObjectLockConfiguration'
  { -- | Indicates whether the bucket has an Object Lock configuration enabled.
    objectLockEnabled :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Object Lock rule for the specified object.
    rule :: Prelude.Maybe AwsS3BucketObjectLockConfigurationRuleDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketObjectLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectLockEnabled', 'awsS3BucketObjectLockConfiguration_objectLockEnabled' - Indicates whether the bucket has an Object Lock configuration enabled.
--
-- 'rule', 'awsS3BucketObjectLockConfiguration_rule' - Specifies the Object Lock rule for the specified object.
newAwsS3BucketObjectLockConfiguration ::
  AwsS3BucketObjectLockConfiguration
newAwsS3BucketObjectLockConfiguration =
  AwsS3BucketObjectLockConfiguration'
    { objectLockEnabled =
        Prelude.Nothing,
      rule = Prelude.Nothing
    }

-- | Indicates whether the bucket has an Object Lock configuration enabled.
awsS3BucketObjectLockConfiguration_objectLockEnabled :: Lens.Lens' AwsS3BucketObjectLockConfiguration (Prelude.Maybe Prelude.Text)
awsS3BucketObjectLockConfiguration_objectLockEnabled = Lens.lens (\AwsS3BucketObjectLockConfiguration' {objectLockEnabled} -> objectLockEnabled) (\s@AwsS3BucketObjectLockConfiguration' {} a -> s {objectLockEnabled = a} :: AwsS3BucketObjectLockConfiguration)

-- | Specifies the Object Lock rule for the specified object.
awsS3BucketObjectLockConfiguration_rule :: Lens.Lens' AwsS3BucketObjectLockConfiguration (Prelude.Maybe AwsS3BucketObjectLockConfigurationRuleDetails)
awsS3BucketObjectLockConfiguration_rule = Lens.lens (\AwsS3BucketObjectLockConfiguration' {rule} -> rule) (\s@AwsS3BucketObjectLockConfiguration' {} a -> s {rule = a} :: AwsS3BucketObjectLockConfiguration)

instance
  Data.FromJSON
    AwsS3BucketObjectLockConfiguration
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketObjectLockConfiguration"
      ( \x ->
          AwsS3BucketObjectLockConfiguration'
            Prelude.<$> (x Data..:? "ObjectLockEnabled")
            Prelude.<*> (x Data..:? "Rule")
      )

instance
  Prelude.Hashable
    AwsS3BucketObjectLockConfiguration
  where
  hashWithSalt
    _salt
    AwsS3BucketObjectLockConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` objectLockEnabled
        `Prelude.hashWithSalt` rule

instance
  Prelude.NFData
    AwsS3BucketObjectLockConfiguration
  where
  rnf AwsS3BucketObjectLockConfiguration' {..} =
    Prelude.rnf objectLockEnabled
      `Prelude.seq` Prelude.rnf rule

instance
  Data.ToJSON
    AwsS3BucketObjectLockConfiguration
  where
  toJSON AwsS3BucketObjectLockConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectLockEnabled" Data..=)
              Prelude.<$> objectLockEnabled,
            ("Rule" Data..=) Prelude.<$> rule
          ]
      )
