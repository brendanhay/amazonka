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
-- Module      : Network.AWS.Redshift.DescribeStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns account level backups storage size and provisional storage.
module Network.AWS.Redshift.DescribeStorage
  ( -- * Creating a Request
    describeStorage,
    DescribeStorage,

    -- * Destructuring the Response
    describeStorageResponse,
    DescribeStorageResponse,

    -- * Response Lenses
    dsrsTotalProvisionedStorageInMegaBytes,
    dsrsTotalBackupSizeInMegaBytes,
    dsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStorage' smart constructor.
data DescribeStorage = DescribeStorage'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStorage' with the minimum fields required to make a request.
describeStorage ::
  DescribeStorage
describeStorage = DescribeStorage'

instance AWSRequest DescribeStorage where
  type Rs DescribeStorage = DescribeStorageResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "DescribeStorageResult"
      ( \s h x ->
          DescribeStorageResponse'
            <$> (x .@? "TotalProvisionedStorageInMegaBytes")
            <*> (x .@? "TotalBackupSizeInMegaBytes")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeStorage

instance NFData DescribeStorage

instance ToHeaders DescribeStorage where
  toHeaders = const mempty

instance ToPath DescribeStorage where
  toPath = const "/"

instance ToQuery DescribeStorage where
  toQuery =
    const
      ( mconcat
          [ "Action" =: ("DescribeStorage" :: ByteString),
            "Version" =: ("2012-12-01" :: ByteString)
          ]
      )

-- | /See:/ 'describeStorageResponse' smart constructor.
data DescribeStorageResponse = DescribeStorageResponse'
  { _dsrsTotalProvisionedStorageInMegaBytes ::
      !(Maybe Double),
    _dsrsTotalBackupSizeInMegaBytes ::
      !(Maybe Double),
    _dsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStorageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsTotalProvisionedStorageInMegaBytes' - The total amount of storage currently provisioned.
--
-- * 'dsrsTotalBackupSizeInMegaBytes' - The total amount of storage currently used for snapshots.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeStorageResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DescribeStorageResponse
describeStorageResponse pResponseStatus_ =
  DescribeStorageResponse'
    { _dsrsTotalProvisionedStorageInMegaBytes =
        Nothing,
      _dsrsTotalBackupSizeInMegaBytes = Nothing,
      _dsrsResponseStatus = pResponseStatus_
    }

-- | The total amount of storage currently provisioned.
dsrsTotalProvisionedStorageInMegaBytes :: Lens' DescribeStorageResponse (Maybe Double)
dsrsTotalProvisionedStorageInMegaBytes = lens _dsrsTotalProvisionedStorageInMegaBytes (\s a -> s {_dsrsTotalProvisionedStorageInMegaBytes = a})

-- | The total amount of storage currently used for snapshots.
dsrsTotalBackupSizeInMegaBytes :: Lens' DescribeStorageResponse (Maybe Double)
dsrsTotalBackupSizeInMegaBytes = lens _dsrsTotalBackupSizeInMegaBytes (\s a -> s {_dsrsTotalBackupSizeInMegaBytes = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeStorageResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DescribeStorageResponse
