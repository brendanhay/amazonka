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
-- Module      : Network.AWS.Glue.GetDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configuration for a specified catalog.
module Network.AWS.Glue.GetDataCatalogEncryptionSettings
  ( -- * Creating a Request
    getDataCatalogEncryptionSettings,
    GetDataCatalogEncryptionSettings,

    -- * Request Lenses
    gdcesCatalogId,

    -- * Destructuring the Response
    getDataCatalogEncryptionSettingsResponse,
    GetDataCatalogEncryptionSettingsResponse,

    -- * Response Lenses
    gdcesrsDataCatalogEncryptionSettings,
    gdcesrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDataCatalogEncryptionSettings' smart constructor.
newtype GetDataCatalogEncryptionSettings = GetDataCatalogEncryptionSettings'
  { _gdcesCatalogId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDataCatalogEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcesCatalogId' - The ID of the Data Catalog to retrieve the security configuration for. If none is provided, the AWS account ID is used by default.
getDataCatalogEncryptionSettings ::
  GetDataCatalogEncryptionSettings
getDataCatalogEncryptionSettings =
  GetDataCatalogEncryptionSettings' {_gdcesCatalogId = Nothing}

-- | The ID of the Data Catalog to retrieve the security configuration for. If none is provided, the AWS account ID is used by default.
gdcesCatalogId :: Lens' GetDataCatalogEncryptionSettings (Maybe Text)
gdcesCatalogId = lens _gdcesCatalogId (\s a -> s {_gdcesCatalogId = a})

instance AWSRequest GetDataCatalogEncryptionSettings where
  type
    Rs GetDataCatalogEncryptionSettings =
      GetDataCatalogEncryptionSettingsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetDataCatalogEncryptionSettingsResponse'
            <$> (x .?> "DataCatalogEncryptionSettings") <*> (pure (fromEnum s))
      )

instance Hashable GetDataCatalogEncryptionSettings

instance NFData GetDataCatalogEncryptionSettings

instance ToHeaders GetDataCatalogEncryptionSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.GetDataCatalogEncryptionSettings" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDataCatalogEncryptionSettings where
  toJSON GetDataCatalogEncryptionSettings' {..} =
    object (catMaybes [("CatalogId" .=) <$> _gdcesCatalogId])

instance ToPath GetDataCatalogEncryptionSettings where
  toPath = const "/"

instance ToQuery GetDataCatalogEncryptionSettings where
  toQuery = const mempty

-- | /See:/ 'getDataCatalogEncryptionSettingsResponse' smart constructor.
data GetDataCatalogEncryptionSettingsResponse = GetDataCatalogEncryptionSettingsResponse'
  { _gdcesrsDataCatalogEncryptionSettings ::
      !( Maybe
           DataCatalogEncryptionSettings
       ),
    _gdcesrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDataCatalogEncryptionSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcesrsDataCatalogEncryptionSettings' - The requested security configuration.
--
-- * 'gdcesrsResponseStatus' - -- | The response status code.
getDataCatalogEncryptionSettingsResponse ::
  -- | 'gdcesrsResponseStatus'
  Int ->
  GetDataCatalogEncryptionSettingsResponse
getDataCatalogEncryptionSettingsResponse pResponseStatus_ =
  GetDataCatalogEncryptionSettingsResponse'
    { _gdcesrsDataCatalogEncryptionSettings =
        Nothing,
      _gdcesrsResponseStatus = pResponseStatus_
    }

-- | The requested security configuration.
gdcesrsDataCatalogEncryptionSettings :: Lens' GetDataCatalogEncryptionSettingsResponse (Maybe DataCatalogEncryptionSettings)
gdcesrsDataCatalogEncryptionSettings = lens _gdcesrsDataCatalogEncryptionSettings (\s a -> s {_gdcesrsDataCatalogEncryptionSettings = a})

-- | -- | The response status code.
gdcesrsResponseStatus :: Lens' GetDataCatalogEncryptionSettingsResponse Int
gdcesrsResponseStatus = lens _gdcesrsResponseStatus (\s a -> s {_gdcesrsResponseStatus = a})

instance NFData GetDataCatalogEncryptionSettingsResponse
