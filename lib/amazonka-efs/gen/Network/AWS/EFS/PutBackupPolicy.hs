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
-- Module      : Network.AWS.EFS.PutBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the file system's backup policy. Use this action to start or stop automatic backups of the file system.
module Network.AWS.EFS.PutBackupPolicy
  ( -- * Creating a Request
    putBackupPolicy,
    PutBackupPolicy,

    -- * Request Lenses
    pbpFileSystemId,
    pbpBackupPolicy,

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

-- | /See:/ 'putBackupPolicy' smart constructor.
data PutBackupPolicy = PutBackupPolicy'
  { _pbpFileSystemId :: !Text,
    _pbpBackupPolicy :: !BackupPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBackupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbpFileSystemId' - Specifies which EFS file system to update the backup policy for.
--
-- * 'pbpBackupPolicy' - The backup policy included in the @PutBackupPolicy@ request.
putBackupPolicy ::
  -- | 'pbpFileSystemId'
  Text ->
  -- | 'pbpBackupPolicy'
  BackupPolicy ->
  PutBackupPolicy
putBackupPolicy pFileSystemId_ pBackupPolicy_ =
  PutBackupPolicy'
    { _pbpFileSystemId = pFileSystemId_,
      _pbpBackupPolicy = pBackupPolicy_
    }

-- | Specifies which EFS file system to update the backup policy for.
pbpFileSystemId :: Lens' PutBackupPolicy Text
pbpFileSystemId = lens _pbpFileSystemId (\s a -> s {_pbpFileSystemId = a})

-- | The backup policy included in the @PutBackupPolicy@ request.
pbpBackupPolicy :: Lens' PutBackupPolicy BackupPolicy
pbpBackupPolicy = lens _pbpBackupPolicy (\s a -> s {_pbpBackupPolicy = a})

instance AWSRequest PutBackupPolicy where
  type Rs PutBackupPolicy = BackupPolicyDescription
  request = putJSON efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable PutBackupPolicy

instance NFData PutBackupPolicy

instance ToHeaders PutBackupPolicy where
  toHeaders = const mempty

instance ToJSON PutBackupPolicy where
  toJSON PutBackupPolicy' {..} =
    object (catMaybes [Just ("BackupPolicy" .= _pbpBackupPolicy)])

instance ToPath PutBackupPolicy where
  toPath PutBackupPolicy' {..} =
    mconcat
      [ "/2015-02-01/file-systems/",
        toBS _pbpFileSystemId,
        "/backup-policy"
      ]

instance ToQuery PutBackupPolicy where
  toQuery = const mempty
