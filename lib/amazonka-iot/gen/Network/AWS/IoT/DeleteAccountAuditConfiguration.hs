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
-- Module      : Network.AWS.IoT.DeleteAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the default settings for Device Defender audits for this account. Any configuration data you entered is deleted and all audit checks are reset to disabled.
module Network.AWS.IoT.DeleteAccountAuditConfiguration
  ( -- * Creating a Request
    deleteAccountAuditConfiguration,
    DeleteAccountAuditConfiguration,

    -- * Request Lenses
    daacDeleteScheduledAudits,

    -- * Destructuring the Response
    deleteAccountAuditConfigurationResponse,
    DeleteAccountAuditConfigurationResponse,

    -- * Response Lenses
    daacarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccountAuditConfiguration' smart constructor.
newtype DeleteAccountAuditConfiguration = DeleteAccountAuditConfiguration'
  { _daacDeleteScheduledAudits ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccountAuditConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daacDeleteScheduledAudits' - If true, all scheduled audits are deleted.
deleteAccountAuditConfiguration ::
  DeleteAccountAuditConfiguration
deleteAccountAuditConfiguration =
  DeleteAccountAuditConfiguration'
    { _daacDeleteScheduledAudits =
        Nothing
    }

-- | If true, all scheduled audits are deleted.
daacDeleteScheduledAudits :: Lens' DeleteAccountAuditConfiguration (Maybe Bool)
daacDeleteScheduledAudits = lens _daacDeleteScheduledAudits (\s a -> s {_daacDeleteScheduledAudits = a})

instance AWSRequest DeleteAccountAuditConfiguration where
  type
    Rs DeleteAccountAuditConfiguration =
      DeleteAccountAuditConfigurationResponse
  request = delete ioT
  response =
    receiveEmpty
      ( \s h x ->
          DeleteAccountAuditConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteAccountAuditConfiguration

instance NFData DeleteAccountAuditConfiguration

instance ToHeaders DeleteAccountAuditConfiguration where
  toHeaders = const mempty

instance ToPath DeleteAccountAuditConfiguration where
  toPath = const "/audit/configuration"

instance ToQuery DeleteAccountAuditConfiguration where
  toQuery DeleteAccountAuditConfiguration' {..} =
    mconcat ["deleteScheduledAudits" =: _daacDeleteScheduledAudits]

-- | /See:/ 'deleteAccountAuditConfigurationResponse' smart constructor.
newtype DeleteAccountAuditConfigurationResponse = DeleteAccountAuditConfigurationResponse'
  { _daacarsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteAccountAuditConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daacarsResponseStatus' - -- | The response status code.
deleteAccountAuditConfigurationResponse ::
  -- | 'daacarsResponseStatus'
  Int ->
  DeleteAccountAuditConfigurationResponse
deleteAccountAuditConfigurationResponse pResponseStatus_ =
  DeleteAccountAuditConfigurationResponse'
    { _daacarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
daacarsResponseStatus :: Lens' DeleteAccountAuditConfigurationResponse Int
daacarsResponseStatus = lens _daacarsResponseStatus (\s a -> s {_daacarsResponseStatus = a})

instance NFData DeleteAccountAuditConfigurationResponse
