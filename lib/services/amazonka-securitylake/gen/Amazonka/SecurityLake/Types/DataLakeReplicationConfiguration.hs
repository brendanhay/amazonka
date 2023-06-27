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
-- Module      : Amazonka.SecurityLake.Types.DataLakeReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides replication details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeReplicationConfiguration' smart constructor.
data DataLakeReplicationConfiguration = DataLakeReplicationConfiguration'
  { -- | Replication enables automatic, asynchronous copying of objects across
    -- Amazon S3 buckets. Amazon S3 buckets that are configured for object
    -- replication can be owned by the same Amazon Web Services account or by
    -- different accounts. You can replicate objects to a single destination
    -- bucket or to multiple destination buckets. The destination buckets can
    -- be in different Amazon Web Services Regions or within the same Region as
    -- the source bucket.
    --
    -- Set up one or more rollup Regions by providing the Region or Regions
    -- that should contribute to the central rollup Region.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | Replication settings for the Amazon S3 buckets. This parameter uses the
    -- Identity and Access Management (IAM) role you created that is managed by
    -- Security Lake, to ensure the replication setting is correct.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'dataLakeReplicationConfiguration_regions' - Replication enables automatic, asynchronous copying of objects across
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
-- 'roleArn', 'dataLakeReplicationConfiguration_roleArn' - Replication settings for the Amazon S3 buckets. This parameter uses the
-- Identity and Access Management (IAM) role you created that is managed by
-- Security Lake, to ensure the replication setting is correct.
newDataLakeReplicationConfiguration ::
  DataLakeReplicationConfiguration
newDataLakeReplicationConfiguration =
  DataLakeReplicationConfiguration'
    { regions =
        Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

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
dataLakeReplicationConfiguration_regions :: Lens.Lens' DataLakeReplicationConfiguration (Prelude.Maybe [Prelude.Text])
dataLakeReplicationConfiguration_regions = Lens.lens (\DataLakeReplicationConfiguration' {regions} -> regions) (\s@DataLakeReplicationConfiguration' {} a -> s {regions = a} :: DataLakeReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Replication settings for the Amazon S3 buckets. This parameter uses the
-- Identity and Access Management (IAM) role you created that is managed by
-- Security Lake, to ensure the replication setting is correct.
dataLakeReplicationConfiguration_roleArn :: Lens.Lens' DataLakeReplicationConfiguration (Prelude.Maybe Prelude.Text)
dataLakeReplicationConfiguration_roleArn = Lens.lens (\DataLakeReplicationConfiguration' {roleArn} -> roleArn) (\s@DataLakeReplicationConfiguration' {} a -> s {roleArn = a} :: DataLakeReplicationConfiguration)

instance
  Data.FromJSON
    DataLakeReplicationConfiguration
  where
  parseJSON =
    Data.withObject
      "DataLakeReplicationConfiguration"
      ( \x ->
          DataLakeReplicationConfiguration'
            Prelude.<$> (x Data..:? "regions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "roleArn")
      )

instance
  Prelude.Hashable
    DataLakeReplicationConfiguration
  where
  hashWithSalt
    _salt
    DataLakeReplicationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` regions
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    DataLakeReplicationConfiguration
  where
  rnf DataLakeReplicationConfiguration' {..} =
    Prelude.rnf regions
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON DataLakeReplicationConfiguration where
  toJSON DataLakeReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("regions" Data..=) Prelude.<$> regions,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )
