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
-- Module      : Network.AWS.CloudHSMv2.ModifyBackupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes for AWS CloudHSM backup.
module Network.AWS.CloudHSMv2.ModifyBackupAttributes
  ( -- * Creating a Request
    modifyBackupAttributes,
    ModifyBackupAttributes,

    -- * Request Lenses
    mbaBackupId,
    mbaNeverExpires,

    -- * Destructuring the Response
    modifyBackupAttributesResponse,
    ModifyBackupAttributesResponse,

    -- * Response Lenses
    mbarsBackup,
    mbarsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyBackupAttributes' smart constructor.
data ModifyBackupAttributes = ModifyBackupAttributes'
  { _mbaBackupId ::
      !Text,
    _mbaNeverExpires :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyBackupAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbaBackupId' - The identifier (ID) of the backup to modify. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- * 'mbaNeverExpires' - Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
modifyBackupAttributes ::
  -- | 'mbaBackupId'
  Text ->
  -- | 'mbaNeverExpires'
  Bool ->
  ModifyBackupAttributes
modifyBackupAttributes pBackupId_ pNeverExpires_ =
  ModifyBackupAttributes'
    { _mbaBackupId = pBackupId_,
      _mbaNeverExpires = pNeverExpires_
    }

-- | The identifier (ID) of the backup to modify. To find the ID of a backup, use the 'DescribeBackups' operation.
mbaBackupId :: Lens' ModifyBackupAttributes Text
mbaBackupId = lens _mbaBackupId (\s a -> s {_mbaBackupId = a})

-- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
mbaNeverExpires :: Lens' ModifyBackupAttributes Bool
mbaNeverExpires = lens _mbaNeverExpires (\s a -> s {_mbaNeverExpires = a})

instance AWSRequest ModifyBackupAttributes where
  type Rs ModifyBackupAttributes = ModifyBackupAttributesResponse
  request = postJSON cloudHSMv2
  response =
    receiveJSON
      ( \s h x ->
          ModifyBackupAttributesResponse'
            <$> (x .?> "Backup") <*> (pure (fromEnum s))
      )

instance Hashable ModifyBackupAttributes

instance NFData ModifyBackupAttributes

instance ToHeaders ModifyBackupAttributes where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("BaldrApiService.ModifyBackupAttributes" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyBackupAttributes where
  toJSON ModifyBackupAttributes' {..} =
    object
      ( catMaybes
          [ Just ("BackupId" .= _mbaBackupId),
            Just ("NeverExpires" .= _mbaNeverExpires)
          ]
      )

instance ToPath ModifyBackupAttributes where
  toPath = const "/"

instance ToQuery ModifyBackupAttributes where
  toQuery = const mempty

-- | /See:/ 'modifyBackupAttributesResponse' smart constructor.
data ModifyBackupAttributesResponse = ModifyBackupAttributesResponse'
  { _mbarsBackup ::
      !(Maybe Backup),
    _mbarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyBackupAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbarsBackup' - Undocumented member.
--
-- * 'mbarsResponseStatus' - -- | The response status code.
modifyBackupAttributesResponse ::
  -- | 'mbarsResponseStatus'
  Int ->
  ModifyBackupAttributesResponse
modifyBackupAttributesResponse pResponseStatus_ =
  ModifyBackupAttributesResponse'
    { _mbarsBackup = Nothing,
      _mbarsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mbarsBackup :: Lens' ModifyBackupAttributesResponse (Maybe Backup)
mbarsBackup = lens _mbarsBackup (\s a -> s {_mbarsBackup = a})

-- | -- | The response status code.
mbarsResponseStatus :: Lens' ModifyBackupAttributesResponse Int
mbarsResponseStatus = lens _mbarsResponseStatus (\s a -> s {_mbarsResponseStatus = a})

instance NFData ModifyBackupAttributesResponse
