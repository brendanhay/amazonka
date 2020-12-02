{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A resource data sync helps you view data from multiple sources in a single location. Systems Manager offers two types of resource data sync: @SyncToDestination@ and @SyncFromSource@ .
--
--
-- You can configure Systems Manager Inventory to use the @SyncToDestination@ type to synchronize Inventory data from multiple AWS Regions to a single S3 bucket. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-datasync.html Configuring Resource Data Sync for Inventory> in the /AWS Systems Manager User Guide/ .
--
-- You can configure Systems Manager Explorer to use the @SyncFromSource@ type to synchronize operational work items (OpsItems) and operational data (OpsData) from multiple AWS Regions to a single S3 bucket. This type can synchronize OpsItems and OpsData from multiple AWS accounts and Regions or @EntireOrganization@ by using AWS Organizations. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resource-data-sync.html Setting up Systems Manager Explorer to display data from multiple accounts and Regions> in the /AWS Systems Manager User Guide/ .
--
-- A resource data sync is an asynchronous operation that returns immediately. After a successful initial sync is completed, the system continuously syncs data. To check the status of a sync, use the 'ListResourceDataSync' .
module Network.AWS.SSM.CreateResourceDataSync
  ( -- * Creating a Request
    createResourceDataSync,
    CreateResourceDataSync,

    -- * Request Lenses
    crdsSyncType,
    crdsSyncSource,
    crdsS3Destination,
    crdsSyncName,

    -- * Destructuring the Response
    createResourceDataSyncResponse,
    CreateResourceDataSyncResponse,

    -- * Response Lenses
    crdsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'createResourceDataSync' smart constructor.
data CreateResourceDataSync = CreateResourceDataSync'
  { _crdsSyncType ::
      !(Maybe Text),
    _crdsSyncSource ::
      !(Maybe ResourceDataSyncSource),
    _crdsS3Destination ::
      !(Maybe ResourceDataSyncS3Destination),
    _crdsSyncName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateResourceDataSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsSyncType' - Specify @SyncToDestination@ to create a resource data sync that synchronizes data to an S3 bucket for Inventory. If you specify @SyncToDestination@ , you must provide a value for @S3Destination@ . Specify @SyncFromSource@ to synchronize data from a single account and multiple Regions, or multiple AWS accounts and Regions, as listed in AWS Organizations for Explorer. If you specify @SyncFromSource@ , you must provide a value for @SyncSource@ . The default value is @SyncToDestination@ .
--
-- * 'crdsSyncSource' - Specify information about the data sources to synchronize. This parameter is required if the @SyncType@ value is SyncFromSource.
--
-- * 'crdsS3Destination' - Amazon S3 configuration details for the sync. This parameter is required if the @SyncType@ value is SyncToDestination.
--
-- * 'crdsSyncName' - A name for the configuration.
createResourceDataSync ::
  -- | 'crdsSyncName'
  Text ->
  CreateResourceDataSync
createResourceDataSync pSyncName_ =
  CreateResourceDataSync'
    { _crdsSyncType = Nothing,
      _crdsSyncSource = Nothing,
      _crdsS3Destination = Nothing,
      _crdsSyncName = pSyncName_
    }

-- | Specify @SyncToDestination@ to create a resource data sync that synchronizes data to an S3 bucket for Inventory. If you specify @SyncToDestination@ , you must provide a value for @S3Destination@ . Specify @SyncFromSource@ to synchronize data from a single account and multiple Regions, or multiple AWS accounts and Regions, as listed in AWS Organizations for Explorer. If you specify @SyncFromSource@ , you must provide a value for @SyncSource@ . The default value is @SyncToDestination@ .
crdsSyncType :: Lens' CreateResourceDataSync (Maybe Text)
crdsSyncType = lens _crdsSyncType (\s a -> s {_crdsSyncType = a})

-- | Specify information about the data sources to synchronize. This parameter is required if the @SyncType@ value is SyncFromSource.
crdsSyncSource :: Lens' CreateResourceDataSync (Maybe ResourceDataSyncSource)
crdsSyncSource = lens _crdsSyncSource (\s a -> s {_crdsSyncSource = a})

-- | Amazon S3 configuration details for the sync. This parameter is required if the @SyncType@ value is SyncToDestination.
crdsS3Destination :: Lens' CreateResourceDataSync (Maybe ResourceDataSyncS3Destination)
crdsS3Destination = lens _crdsS3Destination (\s a -> s {_crdsS3Destination = a})

-- | A name for the configuration.
crdsSyncName :: Lens' CreateResourceDataSync Text
crdsSyncName = lens _crdsSyncName (\s a -> s {_crdsSyncName = a})

instance AWSRequest CreateResourceDataSync where
  type Rs CreateResourceDataSync = CreateResourceDataSyncResponse
  request = postJSON ssm
  response =
    receiveEmpty
      ( \s h x ->
          CreateResourceDataSyncResponse' <$> (pure (fromEnum s))
      )

instance Hashable CreateResourceDataSync

instance NFData CreateResourceDataSync

instance ToHeaders CreateResourceDataSync where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.CreateResourceDataSync" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateResourceDataSync where
  toJSON CreateResourceDataSync' {..} =
    object
      ( catMaybes
          [ ("SyncType" .=) <$> _crdsSyncType,
            ("SyncSource" .=) <$> _crdsSyncSource,
            ("S3Destination" .=) <$> _crdsS3Destination,
            Just ("SyncName" .= _crdsSyncName)
          ]
      )

instance ToPath CreateResourceDataSync where
  toPath = const "/"

instance ToQuery CreateResourceDataSync where
  toQuery = const mempty

-- | /See:/ 'createResourceDataSyncResponse' smart constructor.
newtype CreateResourceDataSyncResponse = CreateResourceDataSyncResponse'
  { _crdsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsrsResponseStatus' - -- | The response status code.
createResourceDataSyncResponse ::
  -- | 'crdsrsResponseStatus'
  Int ->
  CreateResourceDataSyncResponse
createResourceDataSyncResponse pResponseStatus_ =
  CreateResourceDataSyncResponse'
    { _crdsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
crdsrsResponseStatus :: Lens' CreateResourceDataSyncResponse Int
crdsrsResponseStatus = lens _crdsrsResponseStatus (\s a -> s {_crdsrsResponseStatus = a})

instance NFData CreateResourceDataSyncResponse
