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
-- Module      : Network.AWS.SMS.DeleteAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the validation configuration for the specified application.
module Network.AWS.SMS.DeleteAppValidationConfiguration
  ( -- * Creating a Request
    deleteAppValidationConfiguration,
    DeleteAppValidationConfiguration,

    -- * Request Lenses
    davcAppId,

    -- * Destructuring the Response
    deleteAppValidationConfigurationResponse,
    DeleteAppValidationConfigurationResponse,

    -- * Response Lenses
    davcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'deleteAppValidationConfiguration' smart constructor.
newtype DeleteAppValidationConfiguration = DeleteAppValidationConfiguration'
  { _davcAppId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppValidationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davcAppId' - The ID of the application.
deleteAppValidationConfiguration ::
  -- | 'davcAppId'
  Text ->
  DeleteAppValidationConfiguration
deleteAppValidationConfiguration pAppId_ =
  DeleteAppValidationConfiguration' {_davcAppId = pAppId_}

-- | The ID of the application.
davcAppId :: Lens' DeleteAppValidationConfiguration Text
davcAppId = lens _davcAppId (\s a -> s {_davcAppId = a})

instance AWSRequest DeleteAppValidationConfiguration where
  type
    Rs DeleteAppValidationConfiguration =
      DeleteAppValidationConfigurationResponse
  request = postJSON sms
  response =
    receiveEmpty
      ( \s h x ->
          DeleteAppValidationConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteAppValidationConfiguration

instance NFData DeleteAppValidationConfiguration

instance ToHeaders DeleteAppValidationConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.DeleteAppValidationConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAppValidationConfiguration where
  toJSON DeleteAppValidationConfiguration' {..} =
    object (catMaybes [Just ("appId" .= _davcAppId)])

instance ToPath DeleteAppValidationConfiguration where
  toPath = const "/"

instance ToQuery DeleteAppValidationConfiguration where
  toQuery = const mempty

-- | /See:/ 'deleteAppValidationConfigurationResponse' smart constructor.
newtype DeleteAppValidationConfigurationResponse = DeleteAppValidationConfigurationResponse'
  { _davcrsResponseStatus ::
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

-- | Creates a value of 'DeleteAppValidationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davcrsResponseStatus' - -- | The response status code.
deleteAppValidationConfigurationResponse ::
  -- | 'davcrsResponseStatus'
  Int ->
  DeleteAppValidationConfigurationResponse
deleteAppValidationConfigurationResponse pResponseStatus_ =
  DeleteAppValidationConfigurationResponse'
    { _davcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
davcrsResponseStatus :: Lens' DeleteAppValidationConfigurationResponse Int
davcrsResponseStatus = lens _davcrsResponseStatus (\s a -> s {_davcrsResponseStatus = a})

instance NFData DeleteAppValidationConfigurationResponse
