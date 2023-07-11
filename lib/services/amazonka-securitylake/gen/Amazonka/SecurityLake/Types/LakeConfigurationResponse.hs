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
-- Module      : Amazonka.SecurityLake.Types.LakeConfigurationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.LakeConfigurationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.Region
import Amazonka.SecurityLake.Types.RetentionSetting
import Amazonka.SecurityLake.Types.SettingsStatus

-- | Provides details of Amazon Security Lake lake configuration object.
--
-- /See:/ 'newLakeConfigurationResponse' smart constructor.
data LakeConfigurationResponse = LakeConfigurationResponse'
  { -- | The type of encryption key used by secure the Security Lake
    -- configuration object.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Replication enables automatic, asynchronous copying of objects across
    -- Amazon S3 buckets. Amazon S3 buckets that are configured for object
    -- replication can be owned by the same Amazon Web Services account or by
    -- different accounts. You can replicate objects to a single destination
    -- bucket or to multiple destination buckets. The destination buckets can
    -- be in different Amazon Web Services Regions or within the same Region as
    -- the source bucket.
    --
    -- Set up one or more rollup Regions by providing the Region or Regions
    -- that should contribute to the central rollup Region.
    replicationDestinationRegions :: Prelude.Maybe [Region],
    -- | Replication settings for the Amazon S3 buckets. This parameter uses the
    -- IAM role you created that is managed by Security Lake, to ensure the
    -- replication setting is correct.
    replicationRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Retention settings for the destination Amazon S3 buckets.
    retentionSettings :: Prelude.Maybe [RetentionSetting],
    -- | Amazon Resource Names (ARNs) uniquely identify Amazon Web Services
    -- resources. Security Lake requires an ARN when you need to specify a
    -- resource unambiguously across all of Amazon Web Services, such as in IAM
    -- policies, Amazon Relational Database Service (Amazon RDS) tags, and API
    -- calls.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | Retrieves the status of the configuration operation for an account in
    -- Amazon Security Lake.
    status :: Prelude.Maybe SettingsStatus,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value, both of which you
    -- define.
    tagsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LakeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'lakeConfigurationResponse_encryptionKey' - The type of encryption key used by secure the Security Lake
-- configuration object.
--
-- 'replicationDestinationRegions', 'lakeConfigurationResponse_replicationDestinationRegions' - Replication enables automatic, asynchronous copying of objects across
-- Amazon S3 buckets. Amazon S3 buckets that are configured for object
-- replication can be owned by the same Amazon Web Services account or by
-- different accounts. You can replicate objects to a single destination
-- bucket or to multiple destination buckets. The destination buckets can
-- be in different Amazon Web Services Regions or within the same Region as
-- the source bucket.
--
-- Set up one or more rollup Regions by providing the Region or Regions
-- that should contribute to the central rollup Region.
--
-- 'replicationRoleArn', 'lakeConfigurationResponse_replicationRoleArn' - Replication settings for the Amazon S3 buckets. This parameter uses the
-- IAM role you created that is managed by Security Lake, to ensure the
-- replication setting is correct.
--
-- 'retentionSettings', 'lakeConfigurationResponse_retentionSettings' - Retention settings for the destination Amazon S3 buckets.
--
-- 's3BucketArn', 'lakeConfigurationResponse_s3BucketArn' - Amazon Resource Names (ARNs) uniquely identify Amazon Web Services
-- resources. Security Lake requires an ARN when you need to specify a
-- resource unambiguously across all of Amazon Web Services, such as in IAM
-- policies, Amazon Relational Database Service (Amazon RDS) tags, and API
-- calls.
--
-- 'status', 'lakeConfigurationResponse_status' - Retrieves the status of the configuration operation for an account in
-- Amazon Security Lake.
--
-- 'tagsMap', 'lakeConfigurationResponse_tagsMap' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value, both of which you
-- define.
newLakeConfigurationResponse ::
  LakeConfigurationResponse
newLakeConfigurationResponse =
  LakeConfigurationResponse'
    { encryptionKey =
        Prelude.Nothing,
      replicationDestinationRegions = Prelude.Nothing,
      replicationRoleArn = Prelude.Nothing,
      retentionSettings = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      status = Prelude.Nothing,
      tagsMap = Prelude.Nothing
    }

-- | The type of encryption key used by secure the Security Lake
-- configuration object.
lakeConfigurationResponse_encryptionKey :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe Prelude.Text)
lakeConfigurationResponse_encryptionKey = Lens.lens (\LakeConfigurationResponse' {encryptionKey} -> encryptionKey) (\s@LakeConfigurationResponse' {} a -> s {encryptionKey = a} :: LakeConfigurationResponse)

-- | Replication enables automatic, asynchronous copying of objects across
-- Amazon S3 buckets. Amazon S3 buckets that are configured for object
-- replication can be owned by the same Amazon Web Services account or by
-- different accounts. You can replicate objects to a single destination
-- bucket or to multiple destination buckets. The destination buckets can
-- be in different Amazon Web Services Regions or within the same Region as
-- the source bucket.
--
-- Set up one or more rollup Regions by providing the Region or Regions
-- that should contribute to the central rollup Region.
lakeConfigurationResponse_replicationDestinationRegions :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe [Region])
lakeConfigurationResponse_replicationDestinationRegions = Lens.lens (\LakeConfigurationResponse' {replicationDestinationRegions} -> replicationDestinationRegions) (\s@LakeConfigurationResponse' {} a -> s {replicationDestinationRegions = a} :: LakeConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Replication settings for the Amazon S3 buckets. This parameter uses the
-- IAM role you created that is managed by Security Lake, to ensure the
-- replication setting is correct.
lakeConfigurationResponse_replicationRoleArn :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe Prelude.Text)
lakeConfigurationResponse_replicationRoleArn = Lens.lens (\LakeConfigurationResponse' {replicationRoleArn} -> replicationRoleArn) (\s@LakeConfigurationResponse' {} a -> s {replicationRoleArn = a} :: LakeConfigurationResponse)

-- | Retention settings for the destination Amazon S3 buckets.
lakeConfigurationResponse_retentionSettings :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe [RetentionSetting])
lakeConfigurationResponse_retentionSettings = Lens.lens (\LakeConfigurationResponse' {retentionSettings} -> retentionSettings) (\s@LakeConfigurationResponse' {} a -> s {retentionSettings = a} :: LakeConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Names (ARNs) uniquely identify Amazon Web Services
-- resources. Security Lake requires an ARN when you need to specify a
-- resource unambiguously across all of Amazon Web Services, such as in IAM
-- policies, Amazon Relational Database Service (Amazon RDS) tags, and API
-- calls.
lakeConfigurationResponse_s3BucketArn :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe Prelude.Text)
lakeConfigurationResponse_s3BucketArn = Lens.lens (\LakeConfigurationResponse' {s3BucketArn} -> s3BucketArn) (\s@LakeConfigurationResponse' {} a -> s {s3BucketArn = a} :: LakeConfigurationResponse)

-- | Retrieves the status of the configuration operation for an account in
-- Amazon Security Lake.
lakeConfigurationResponse_status :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe SettingsStatus)
lakeConfigurationResponse_status = Lens.lens (\LakeConfigurationResponse' {status} -> status) (\s@LakeConfigurationResponse' {} a -> s {status = a} :: LakeConfigurationResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value, both of which you
-- define.
lakeConfigurationResponse_tagsMap :: Lens.Lens' LakeConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lakeConfigurationResponse_tagsMap = Lens.lens (\LakeConfigurationResponse' {tagsMap} -> tagsMap) (\s@LakeConfigurationResponse' {} a -> s {tagsMap = a} :: LakeConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LakeConfigurationResponse where
  parseJSON =
    Data.withObject
      "LakeConfigurationResponse"
      ( \x ->
          LakeConfigurationResponse'
            Prelude.<$> (x Data..:? "encryptionKey")
            Prelude.<*> ( x
                            Data..:? "replicationDestinationRegions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "replicationRoleArn")
            Prelude.<*> ( x
                            Data..:? "retentionSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "s3BucketArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tagsMap" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LakeConfigurationResponse where
  hashWithSalt _salt LakeConfigurationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` replicationDestinationRegions
      `Prelude.hashWithSalt` replicationRoleArn
      `Prelude.hashWithSalt` retentionSettings
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tagsMap

instance Prelude.NFData LakeConfigurationResponse where
  rnf LakeConfigurationResponse' {..} =
    Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf replicationDestinationRegions
      `Prelude.seq` Prelude.rnf replicationRoleArn
      `Prelude.seq` Prelude.rnf retentionSettings
      `Prelude.seq` Prelude.rnf s3BucketArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tagsMap
