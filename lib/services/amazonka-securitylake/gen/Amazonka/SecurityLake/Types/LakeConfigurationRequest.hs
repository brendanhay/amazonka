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
-- Module      : Amazonka.SecurityLake.Types.LakeConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.LakeConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.Region
import Amazonka.SecurityLake.Types.RetentionSetting

-- | Provides details of Amazon Security Lake configuration object.
--
-- /See:/ 'newLakeConfigurationRequest' smart constructor.
data LakeConfigurationRequest = LakeConfigurationRequest'
  { -- | The type of encryption key used by Amazon Security Lake to encrypt the
    -- Security Lake configuration object.
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
    -- Identity and Access Management (IAM) role you created that is managed by
    -- Security Lake, to ensure the replication setting is correct.
    replicationRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Retention settings for the destination Amazon S3 buckets.
    retentionSettings :: Prelude.Maybe [RetentionSetting],
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value, both of which you
    -- define.
    tagsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LakeConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'lakeConfigurationRequest_encryptionKey' - The type of encryption key used by Amazon Security Lake to encrypt the
-- Security Lake configuration object.
--
-- 'replicationDestinationRegions', 'lakeConfigurationRequest_replicationDestinationRegions' - Replication enables automatic, asynchronous copying of objects across
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
-- 'replicationRoleArn', 'lakeConfigurationRequest_replicationRoleArn' - Replication settings for the Amazon S3 buckets. This parameter uses the
-- Identity and Access Management (IAM) role you created that is managed by
-- Security Lake, to ensure the replication setting is correct.
--
-- 'retentionSettings', 'lakeConfigurationRequest_retentionSettings' - Retention settings for the destination Amazon S3 buckets.
--
-- 'tagsMap', 'lakeConfigurationRequest_tagsMap' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value, both of which you
-- define.
newLakeConfigurationRequest ::
  LakeConfigurationRequest
newLakeConfigurationRequest =
  LakeConfigurationRequest'
    { encryptionKey =
        Prelude.Nothing,
      replicationDestinationRegions = Prelude.Nothing,
      replicationRoleArn = Prelude.Nothing,
      retentionSettings = Prelude.Nothing,
      tagsMap = Prelude.Nothing
    }

-- | The type of encryption key used by Amazon Security Lake to encrypt the
-- Security Lake configuration object.
lakeConfigurationRequest_encryptionKey :: Lens.Lens' LakeConfigurationRequest (Prelude.Maybe Prelude.Text)
lakeConfigurationRequest_encryptionKey = Lens.lens (\LakeConfigurationRequest' {encryptionKey} -> encryptionKey) (\s@LakeConfigurationRequest' {} a -> s {encryptionKey = a} :: LakeConfigurationRequest)

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
lakeConfigurationRequest_replicationDestinationRegions :: Lens.Lens' LakeConfigurationRequest (Prelude.Maybe [Region])
lakeConfigurationRequest_replicationDestinationRegions = Lens.lens (\LakeConfigurationRequest' {replicationDestinationRegions} -> replicationDestinationRegions) (\s@LakeConfigurationRequest' {} a -> s {replicationDestinationRegions = a} :: LakeConfigurationRequest) Prelude.. Lens.mapping Lens.coerced

-- | Replication settings for the Amazon S3 buckets. This parameter uses the
-- Identity and Access Management (IAM) role you created that is managed by
-- Security Lake, to ensure the replication setting is correct.
lakeConfigurationRequest_replicationRoleArn :: Lens.Lens' LakeConfigurationRequest (Prelude.Maybe Prelude.Text)
lakeConfigurationRequest_replicationRoleArn = Lens.lens (\LakeConfigurationRequest' {replicationRoleArn} -> replicationRoleArn) (\s@LakeConfigurationRequest' {} a -> s {replicationRoleArn = a} :: LakeConfigurationRequest)

-- | Retention settings for the destination Amazon S3 buckets.
lakeConfigurationRequest_retentionSettings :: Lens.Lens' LakeConfigurationRequest (Prelude.Maybe [RetentionSetting])
lakeConfigurationRequest_retentionSettings = Lens.lens (\LakeConfigurationRequest' {retentionSettings} -> retentionSettings) (\s@LakeConfigurationRequest' {} a -> s {retentionSettings = a} :: LakeConfigurationRequest) Prelude.. Lens.mapping Lens.coerced

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value, both of which you
-- define.
lakeConfigurationRequest_tagsMap :: Lens.Lens' LakeConfigurationRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lakeConfigurationRequest_tagsMap = Lens.lens (\LakeConfigurationRequest' {tagsMap} -> tagsMap) (\s@LakeConfigurationRequest' {} a -> s {tagsMap = a} :: LakeConfigurationRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable LakeConfigurationRequest where
  hashWithSalt _salt LakeConfigurationRequest' {..} =
    _salt `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` replicationDestinationRegions
      `Prelude.hashWithSalt` replicationRoleArn
      `Prelude.hashWithSalt` retentionSettings
      `Prelude.hashWithSalt` tagsMap

instance Prelude.NFData LakeConfigurationRequest where
  rnf LakeConfigurationRequest' {..} =
    Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf replicationDestinationRegions
      `Prelude.seq` Prelude.rnf replicationRoleArn
      `Prelude.seq` Prelude.rnf retentionSettings
      `Prelude.seq` Prelude.rnf tagsMap

instance Data.ToJSON LakeConfigurationRequest where
  toJSON LakeConfigurationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("replicationDestinationRegions" Data..=)
              Prelude.<$> replicationDestinationRegions,
            ("replicationRoleArn" Data..=)
              Prelude.<$> replicationRoleArn,
            ("retentionSettings" Data..=)
              Prelude.<$> retentionSettings,
            ("tagsMap" Data..=) Prelude.<$> tagsMap
          ]
      )
