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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails

-- | Specifies the S3 Object Lock rule for the specified object. In Amazon
-- S3, Object Lock can help prevent objects from being deleted or
-- overwritten for a fixed amount of time or indefinitely.
--
-- /See:/ 'newAwsS3BucketObjectLockConfigurationRuleDetails' smart constructor.
data AwsS3BucketObjectLockConfigurationRuleDetails = AwsS3BucketObjectLockConfigurationRuleDetails'
  { -- | The default Object Lock retention mode and period that you want to apply
    -- to new objects placed in the specified bucket.
    defaultRetention :: Prelude.Maybe AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketObjectLockConfigurationRuleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultRetention', 'awsS3BucketObjectLockConfigurationRuleDetails_defaultRetention' - The default Object Lock retention mode and period that you want to apply
-- to new objects placed in the specified bucket.
newAwsS3BucketObjectLockConfigurationRuleDetails ::
  AwsS3BucketObjectLockConfigurationRuleDetails
newAwsS3BucketObjectLockConfigurationRuleDetails =
  AwsS3BucketObjectLockConfigurationRuleDetails'
    { defaultRetention =
        Prelude.Nothing
    }

-- | The default Object Lock retention mode and period that you want to apply
-- to new objects placed in the specified bucket.
awsS3BucketObjectLockConfigurationRuleDetails_defaultRetention :: Lens.Lens' AwsS3BucketObjectLockConfigurationRuleDetails (Prelude.Maybe AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails)
awsS3BucketObjectLockConfigurationRuleDetails_defaultRetention = Lens.lens (\AwsS3BucketObjectLockConfigurationRuleDetails' {defaultRetention} -> defaultRetention) (\s@AwsS3BucketObjectLockConfigurationRuleDetails' {} a -> s {defaultRetention = a} :: AwsS3BucketObjectLockConfigurationRuleDetails)

instance
  Data.FromJSON
    AwsS3BucketObjectLockConfigurationRuleDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketObjectLockConfigurationRuleDetails"
      ( \x ->
          AwsS3BucketObjectLockConfigurationRuleDetails'
            Prelude.<$> (x Data..:? "DefaultRetention")
      )

instance
  Prelude.Hashable
    AwsS3BucketObjectLockConfigurationRuleDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketObjectLockConfigurationRuleDetails' {..} =
      _salt `Prelude.hashWithSalt` defaultRetention

instance
  Prelude.NFData
    AwsS3BucketObjectLockConfigurationRuleDetails
  where
  rnf
    AwsS3BucketObjectLockConfigurationRuleDetails' {..} =
      Prelude.rnf defaultRetention

instance
  Data.ToJSON
    AwsS3BucketObjectLockConfigurationRuleDetails
  where
  toJSON
    AwsS3BucketObjectLockConfigurationRuleDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DefaultRetention" Data..=)
                Prelude.<$> defaultRetention
            ]
        )
