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
-- Module      : Network.AWS.EFS.DescribeBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup policy for the specified EFS file system.
module Network.AWS.EFS.DescribeBackupPolicy
  ( -- * Creating a Request
    describeBackupPolicy,
    DescribeBackupPolicy,

    -- * Request Lenses
    dbpFileSystemId,

    -- * Destructuring the Response
    backupPolicyDescription,
    BackupPolicyDescription,

    -- * Response Lenses
    bpdBackupPolicy,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBackupPolicy' smart constructor.
newtype DescribeBackupPolicy = DescribeBackupPolicy'
  { _dbpFileSystemId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBackupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpFileSystemId' - Specifies which EFS file system to retrieve the @BackupPolicy@ for.
describeBackupPolicy ::
  -- | 'dbpFileSystemId'
  Text ->
  DescribeBackupPolicy
describeBackupPolicy pFileSystemId_ =
  DescribeBackupPolicy' {_dbpFileSystemId = pFileSystemId_}

-- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
dbpFileSystemId :: Lens' DescribeBackupPolicy Text
dbpFileSystemId = lens _dbpFileSystemId (\s a -> s {_dbpFileSystemId = a})

instance AWSRequest DescribeBackupPolicy where
  type Rs DescribeBackupPolicy = BackupPolicyDescription
  request = get efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable DescribeBackupPolicy

instance NFData DescribeBackupPolicy

instance ToHeaders DescribeBackupPolicy where
  toHeaders = const mempty

instance ToPath DescribeBackupPolicy where
  toPath DescribeBackupPolicy' {..} =
    mconcat
      [ "/2015-02-01/file-systems/",
        toBS _dbpFileSystemId,
        "/backup-policy"
      ]

instance ToQuery DescribeBackupPolicy where
  toQuery = const mempty
