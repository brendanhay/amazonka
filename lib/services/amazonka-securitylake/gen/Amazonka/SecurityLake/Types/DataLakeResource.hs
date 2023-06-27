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
-- Module      : Amazonka.SecurityLake.Types.DataLakeResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.DataLakeEncryptionConfiguration
import Amazonka.SecurityLake.Types.DataLakeLifecycleConfiguration
import Amazonka.SecurityLake.Types.DataLakeReplicationConfiguration
import Amazonka.SecurityLake.Types.DataLakeStatus
import Amazonka.SecurityLake.Types.DataLakeUpdateStatus

-- | Provides details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeResource' smart constructor.
data DataLakeResource = DataLakeResource'
  { -- | Retrieves the status of the configuration operation for an account in
    -- Amazon Security Lake.
    createStatus :: Prelude.Maybe DataLakeStatus,
    -- | Provides encryption details of Amazon Security Lake object.
    encryptionConfiguration :: Prelude.Maybe DataLakeEncryptionConfiguration,
    -- | Provides lifecycle details of Amazon Security Lake object.
    lifecycleConfiguration :: Prelude.Maybe DataLakeLifecycleConfiguration,
    -- | Provides replication details of Amazon Security Lake object.
    replicationConfiguration :: Prelude.Maybe DataLakeReplicationConfiguration,
    -- | The ARN for the Amazon Security Lake Amazon S3 bucket.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the last @UpdateDataLake @or @DeleteDataLake@ API request.
    updateStatus :: Prelude.Maybe DataLakeUpdateStatus,
    -- | The Amazon Resource Name (ARN) created by you to provide to the
    -- subscriber. For more information about ARNs and how to use them in
    -- policies, see the
    -- <https://docs.aws.amazon.com/security-lake/latest/userguide/subscriber-management.html Amazon Security Lake User Guide>.
    dataLakeArn :: Prelude.Text,
    -- | The Amazon Web Services Regions where Security Lake is enabled.
    region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createStatus', 'dataLakeResource_createStatus' - Retrieves the status of the configuration operation for an account in
-- Amazon Security Lake.
--
-- 'encryptionConfiguration', 'dataLakeResource_encryptionConfiguration' - Provides encryption details of Amazon Security Lake object.
--
-- 'lifecycleConfiguration', 'dataLakeResource_lifecycleConfiguration' - Provides lifecycle details of Amazon Security Lake object.
--
-- 'replicationConfiguration', 'dataLakeResource_replicationConfiguration' - Provides replication details of Amazon Security Lake object.
--
-- 's3BucketArn', 'dataLakeResource_s3BucketArn' - The ARN for the Amazon Security Lake Amazon S3 bucket.
--
-- 'updateStatus', 'dataLakeResource_updateStatus' - The status of the last @UpdateDataLake @or @DeleteDataLake@ API request.
--
-- 'dataLakeArn', 'dataLakeResource_dataLakeArn' - The Amazon Resource Name (ARN) created by you to provide to the
-- subscriber. For more information about ARNs and how to use them in
-- policies, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/subscriber-management.html Amazon Security Lake User Guide>.
--
-- 'region', 'dataLakeResource_region' - The Amazon Web Services Regions where Security Lake is enabled.
newDataLakeResource ::
  -- | 'dataLakeArn'
  Prelude.Text ->
  -- | 'region'
  Prelude.Text ->
  DataLakeResource
newDataLakeResource pDataLakeArn_ pRegion_ =
  DataLakeResource'
    { createStatus = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      lifecycleConfiguration = Prelude.Nothing,
      replicationConfiguration = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      dataLakeArn = pDataLakeArn_,
      region = pRegion_
    }

-- | Retrieves the status of the configuration operation for an account in
-- Amazon Security Lake.
dataLakeResource_createStatus :: Lens.Lens' DataLakeResource (Prelude.Maybe DataLakeStatus)
dataLakeResource_createStatus = Lens.lens (\DataLakeResource' {createStatus} -> createStatus) (\s@DataLakeResource' {} a -> s {createStatus = a} :: DataLakeResource)

-- | Provides encryption details of Amazon Security Lake object.
dataLakeResource_encryptionConfiguration :: Lens.Lens' DataLakeResource (Prelude.Maybe DataLakeEncryptionConfiguration)
dataLakeResource_encryptionConfiguration = Lens.lens (\DataLakeResource' {encryptionConfiguration} -> encryptionConfiguration) (\s@DataLakeResource' {} a -> s {encryptionConfiguration = a} :: DataLakeResource)

-- | Provides lifecycle details of Amazon Security Lake object.
dataLakeResource_lifecycleConfiguration :: Lens.Lens' DataLakeResource (Prelude.Maybe DataLakeLifecycleConfiguration)
dataLakeResource_lifecycleConfiguration = Lens.lens (\DataLakeResource' {lifecycleConfiguration} -> lifecycleConfiguration) (\s@DataLakeResource' {} a -> s {lifecycleConfiguration = a} :: DataLakeResource)

-- | Provides replication details of Amazon Security Lake object.
dataLakeResource_replicationConfiguration :: Lens.Lens' DataLakeResource (Prelude.Maybe DataLakeReplicationConfiguration)
dataLakeResource_replicationConfiguration = Lens.lens (\DataLakeResource' {replicationConfiguration} -> replicationConfiguration) (\s@DataLakeResource' {} a -> s {replicationConfiguration = a} :: DataLakeResource)

-- | The ARN for the Amazon Security Lake Amazon S3 bucket.
dataLakeResource_s3BucketArn :: Lens.Lens' DataLakeResource (Prelude.Maybe Prelude.Text)
dataLakeResource_s3BucketArn = Lens.lens (\DataLakeResource' {s3BucketArn} -> s3BucketArn) (\s@DataLakeResource' {} a -> s {s3BucketArn = a} :: DataLakeResource)

-- | The status of the last @UpdateDataLake @or @DeleteDataLake@ API request.
dataLakeResource_updateStatus :: Lens.Lens' DataLakeResource (Prelude.Maybe DataLakeUpdateStatus)
dataLakeResource_updateStatus = Lens.lens (\DataLakeResource' {updateStatus} -> updateStatus) (\s@DataLakeResource' {} a -> s {updateStatus = a} :: DataLakeResource)

-- | The Amazon Resource Name (ARN) created by you to provide to the
-- subscriber. For more information about ARNs and how to use them in
-- policies, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/subscriber-management.html Amazon Security Lake User Guide>.
dataLakeResource_dataLakeArn :: Lens.Lens' DataLakeResource Prelude.Text
dataLakeResource_dataLakeArn = Lens.lens (\DataLakeResource' {dataLakeArn} -> dataLakeArn) (\s@DataLakeResource' {} a -> s {dataLakeArn = a} :: DataLakeResource)

-- | The Amazon Web Services Regions where Security Lake is enabled.
dataLakeResource_region :: Lens.Lens' DataLakeResource Prelude.Text
dataLakeResource_region = Lens.lens (\DataLakeResource' {region} -> region) (\s@DataLakeResource' {} a -> s {region = a} :: DataLakeResource)

instance Data.FromJSON DataLakeResource where
  parseJSON =
    Data.withObject
      "DataLakeResource"
      ( \x ->
          DataLakeResource'
            Prelude.<$> (x Data..:? "createStatus")
            Prelude.<*> (x Data..:? "encryptionConfiguration")
            Prelude.<*> (x Data..:? "lifecycleConfiguration")
            Prelude.<*> (x Data..:? "replicationConfiguration")
            Prelude.<*> (x Data..:? "s3BucketArn")
            Prelude.<*> (x Data..:? "updateStatus")
            Prelude.<*> (x Data..: "dataLakeArn")
            Prelude.<*> (x Data..: "region")
      )

instance Prelude.Hashable DataLakeResource where
  hashWithSalt _salt DataLakeResource' {..} =
    _salt
      `Prelude.hashWithSalt` createStatus
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` lifecycleConfiguration
      `Prelude.hashWithSalt` replicationConfiguration
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` updateStatus
      `Prelude.hashWithSalt` dataLakeArn
      `Prelude.hashWithSalt` region

instance Prelude.NFData DataLakeResource where
  rnf DataLakeResource' {..} =
    Prelude.rnf createStatus
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf lifecycleConfiguration
      `Prelude.seq` Prelude.rnf replicationConfiguration
      `Prelude.seq` Prelude.rnf s3BucketArn
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf dataLakeArn
      `Prelude.seq` Prelude.rnf region
