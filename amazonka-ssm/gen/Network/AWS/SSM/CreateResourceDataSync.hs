{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateResourceDataSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A resource data sync helps you view data from multiple sources in a
-- single location. Systems Manager offers two types of resource data sync:
-- @SyncToDestination@ and @SyncFromSource@.
--
-- You can configure Systems Manager Inventory to use the
-- @SyncToDestination@ type to synchronize Inventory data from multiple AWS
-- Regions to a single S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-datasync.html Configuring Resource Data Sync for Inventory>
-- in the /AWS Systems Manager User Guide/.
--
-- You can configure Systems Manager Explorer to use the @SyncFromSource@
-- type to synchronize operational work items (OpsItems) and operational
-- data (OpsData) from multiple AWS Regions to a single S3 bucket. This
-- type can synchronize OpsItems and OpsData from multiple AWS accounts and
-- Regions or @EntireOrganization@ by using AWS Organizations. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resource-data-sync.html Setting up Systems Manager Explorer to display data from multiple accounts and Regions>
-- in the /AWS Systems Manager User Guide/.
--
-- A resource data sync is an asynchronous operation that returns
-- immediately. After a successful initial sync is completed, the system
-- continuously syncs data. To check the status of a sync, use the
-- ListResourceDataSync.
--
-- By default, data is not encrypted in Amazon S3. We strongly recommend
-- that you enable encryption in Amazon S3 to ensure secure data storage.
-- We also recommend that you secure access to the Amazon S3 bucket by
-- creating a restrictive bucket policy.
module Network.AWS.SSM.CreateResourceDataSync
  ( -- * Creating a Request
    CreateResourceDataSync (..),
    newCreateResourceDataSync,

    -- * Request Lenses
    createResourceDataSync_syncType,
    createResourceDataSync_s3Destination,
    createResourceDataSync_syncSource,
    createResourceDataSync_syncName,

    -- * Destructuring the Response
    CreateResourceDataSyncResponse (..),
    newCreateResourceDataSyncResponse,

    -- * Response Lenses
    createResourceDataSyncResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateResourceDataSync' smart constructor.
data CreateResourceDataSync = CreateResourceDataSync'
  { -- | Specify @SyncToDestination@ to create a resource data sync that
    -- synchronizes data to an S3 bucket for Inventory. If you specify
    -- @SyncToDestination@, you must provide a value for @S3Destination@.
    -- Specify @SyncFromSource@ to synchronize data from a single account and
    -- multiple Regions, or multiple AWS accounts and Regions, as listed in AWS
    -- Organizations for Explorer. If you specify @SyncFromSource@, you must
    -- provide a value for @SyncSource@. The default value is
    -- @SyncToDestination@.
    syncType :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 configuration details for the sync. This parameter is required
    -- if the @SyncType@ value is SyncToDestination.
    s3Destination :: Prelude.Maybe ResourceDataSyncS3Destination,
    -- | Specify information about the data sources to synchronize. This
    -- parameter is required if the @SyncType@ value is SyncFromSource.
    syncSource :: Prelude.Maybe ResourceDataSyncSource,
    -- | A name for the configuration.
    syncName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDataSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncType', 'createResourceDataSync_syncType' - Specify @SyncToDestination@ to create a resource data sync that
-- synchronizes data to an S3 bucket for Inventory. If you specify
-- @SyncToDestination@, you must provide a value for @S3Destination@.
-- Specify @SyncFromSource@ to synchronize data from a single account and
-- multiple Regions, or multiple AWS accounts and Regions, as listed in AWS
-- Organizations for Explorer. If you specify @SyncFromSource@, you must
-- provide a value for @SyncSource@. The default value is
-- @SyncToDestination@.
--
-- 's3Destination', 'createResourceDataSync_s3Destination' - Amazon S3 configuration details for the sync. This parameter is required
-- if the @SyncType@ value is SyncToDestination.
--
-- 'syncSource', 'createResourceDataSync_syncSource' - Specify information about the data sources to synchronize. This
-- parameter is required if the @SyncType@ value is SyncFromSource.
--
-- 'syncName', 'createResourceDataSync_syncName' - A name for the configuration.
newCreateResourceDataSync ::
  -- | 'syncName'
  Prelude.Text ->
  CreateResourceDataSync
newCreateResourceDataSync pSyncName_ =
  CreateResourceDataSync'
    { syncType = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      syncSource = Prelude.Nothing,
      syncName = pSyncName_
    }

-- | Specify @SyncToDestination@ to create a resource data sync that
-- synchronizes data to an S3 bucket for Inventory. If you specify
-- @SyncToDestination@, you must provide a value for @S3Destination@.
-- Specify @SyncFromSource@ to synchronize data from a single account and
-- multiple Regions, or multiple AWS accounts and Regions, as listed in AWS
-- Organizations for Explorer. If you specify @SyncFromSource@, you must
-- provide a value for @SyncSource@. The default value is
-- @SyncToDestination@.
createResourceDataSync_syncType :: Lens.Lens' CreateResourceDataSync (Prelude.Maybe Prelude.Text)
createResourceDataSync_syncType = Lens.lens (\CreateResourceDataSync' {syncType} -> syncType) (\s@CreateResourceDataSync' {} a -> s {syncType = a} :: CreateResourceDataSync)

-- | Amazon S3 configuration details for the sync. This parameter is required
-- if the @SyncType@ value is SyncToDestination.
createResourceDataSync_s3Destination :: Lens.Lens' CreateResourceDataSync (Prelude.Maybe ResourceDataSyncS3Destination)
createResourceDataSync_s3Destination = Lens.lens (\CreateResourceDataSync' {s3Destination} -> s3Destination) (\s@CreateResourceDataSync' {} a -> s {s3Destination = a} :: CreateResourceDataSync)

-- | Specify information about the data sources to synchronize. This
-- parameter is required if the @SyncType@ value is SyncFromSource.
createResourceDataSync_syncSource :: Lens.Lens' CreateResourceDataSync (Prelude.Maybe ResourceDataSyncSource)
createResourceDataSync_syncSource = Lens.lens (\CreateResourceDataSync' {syncSource} -> syncSource) (\s@CreateResourceDataSync' {} a -> s {syncSource = a} :: CreateResourceDataSync)

-- | A name for the configuration.
createResourceDataSync_syncName :: Lens.Lens' CreateResourceDataSync Prelude.Text
createResourceDataSync_syncName = Lens.lens (\CreateResourceDataSync' {syncName} -> syncName) (\s@CreateResourceDataSync' {} a -> s {syncName = a} :: CreateResourceDataSync)

instance Prelude.AWSRequest CreateResourceDataSync where
  type
    Rs CreateResourceDataSync =
      CreateResourceDataSyncResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateResourceDataSyncResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResourceDataSync

instance Prelude.NFData CreateResourceDataSync

instance Prelude.ToHeaders CreateResourceDataSync where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.CreateResourceDataSync" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateResourceDataSync where
  toJSON CreateResourceDataSync' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SyncType" Prelude..=) Prelude.<$> syncType,
            ("S3Destination" Prelude..=)
              Prelude.<$> s3Destination,
            ("SyncSource" Prelude..=) Prelude.<$> syncSource,
            Prelude.Just ("SyncName" Prelude..= syncName)
          ]
      )

instance Prelude.ToPath CreateResourceDataSync where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateResourceDataSync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceDataSyncResponse' smart constructor.
data CreateResourceDataSyncResponse = CreateResourceDataSyncResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDataSyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createResourceDataSyncResponse_httpStatus' - The response's http status code.
newCreateResourceDataSyncResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceDataSyncResponse
newCreateResourceDataSyncResponse pHttpStatus_ =
  CreateResourceDataSyncResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createResourceDataSyncResponse_httpStatus :: Lens.Lens' CreateResourceDataSyncResponse Prelude.Int
createResourceDataSyncResponse_httpStatus = Lens.lens (\CreateResourceDataSyncResponse' {httpStatus} -> httpStatus) (\s@CreateResourceDataSyncResponse' {} a -> s {httpStatus = a} :: CreateResourceDataSyncResponse)

instance
  Prelude.NFData
    CreateResourceDataSyncResponse
