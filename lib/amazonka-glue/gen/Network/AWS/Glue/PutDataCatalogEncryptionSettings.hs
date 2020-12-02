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
-- Module      : Network.AWS.Glue.PutDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the security configuration for a specified catalog. After the configuration has been set, the specified encryption is applied to every catalog write thereafter.
module Network.AWS.Glue.PutDataCatalogEncryptionSettings
  ( -- * Creating a Request
    putDataCatalogEncryptionSettings,
    PutDataCatalogEncryptionSettings,

    -- * Request Lenses
    pdcesCatalogId,
    pdcesDataCatalogEncryptionSettings,

    -- * Destructuring the Response
    putDataCatalogEncryptionSettingsResponse,
    PutDataCatalogEncryptionSettingsResponse,

    -- * Response Lenses
    pdcesrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putDataCatalogEncryptionSettings' smart constructor.
data PutDataCatalogEncryptionSettings = PutDataCatalogEncryptionSettings'
  { _pdcesCatalogId ::
      !(Maybe Text),
    _pdcesDataCatalogEncryptionSettings ::
      !DataCatalogEncryptionSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutDataCatalogEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdcesCatalogId' - The ID of the Data Catalog to set the security configuration for. If none is provided, the AWS account ID is used by default.
--
-- * 'pdcesDataCatalogEncryptionSettings' - The security configuration to set.
putDataCatalogEncryptionSettings ::
  -- | 'pdcesDataCatalogEncryptionSettings'
  DataCatalogEncryptionSettings ->
  PutDataCatalogEncryptionSettings
putDataCatalogEncryptionSettings pDataCatalogEncryptionSettings_ =
  PutDataCatalogEncryptionSettings'
    { _pdcesCatalogId = Nothing,
      _pdcesDataCatalogEncryptionSettings =
        pDataCatalogEncryptionSettings_
    }

-- | The ID of the Data Catalog to set the security configuration for. If none is provided, the AWS account ID is used by default.
pdcesCatalogId :: Lens' PutDataCatalogEncryptionSettings (Maybe Text)
pdcesCatalogId = lens _pdcesCatalogId (\s a -> s {_pdcesCatalogId = a})

-- | The security configuration to set.
pdcesDataCatalogEncryptionSettings :: Lens' PutDataCatalogEncryptionSettings DataCatalogEncryptionSettings
pdcesDataCatalogEncryptionSettings = lens _pdcesDataCatalogEncryptionSettings (\s a -> s {_pdcesDataCatalogEncryptionSettings = a})

instance AWSRequest PutDataCatalogEncryptionSettings where
  type
    Rs PutDataCatalogEncryptionSettings =
      PutDataCatalogEncryptionSettingsResponse
  request = postJSON glue
  response =
    receiveEmpty
      ( \s h x ->
          PutDataCatalogEncryptionSettingsResponse' <$> (pure (fromEnum s))
      )

instance Hashable PutDataCatalogEncryptionSettings

instance NFData PutDataCatalogEncryptionSettings

instance ToHeaders PutDataCatalogEncryptionSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.PutDataCatalogEncryptionSettings" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutDataCatalogEncryptionSettings where
  toJSON PutDataCatalogEncryptionSettings' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _pdcesCatalogId,
            Just
              ( "DataCatalogEncryptionSettings"
                  .= _pdcesDataCatalogEncryptionSettings
              )
          ]
      )

instance ToPath PutDataCatalogEncryptionSettings where
  toPath = const "/"

instance ToQuery PutDataCatalogEncryptionSettings where
  toQuery = const mempty

-- | /See:/ 'putDataCatalogEncryptionSettingsResponse' smart constructor.
newtype PutDataCatalogEncryptionSettingsResponse = PutDataCatalogEncryptionSettingsResponse'
  { _pdcesrsResponseStatus ::
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

-- | Creates a value of 'PutDataCatalogEncryptionSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdcesrsResponseStatus' - -- | The response status code.
putDataCatalogEncryptionSettingsResponse ::
  -- | 'pdcesrsResponseStatus'
  Int ->
  PutDataCatalogEncryptionSettingsResponse
putDataCatalogEncryptionSettingsResponse pResponseStatus_ =
  PutDataCatalogEncryptionSettingsResponse'
    { _pdcesrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
pdcesrsResponseStatus :: Lens' PutDataCatalogEncryptionSettingsResponse Int
pdcesrsResponseStatus = lens _pdcesrsResponseStatus (\s a -> s {_pdcesrsResponseStatus = a})

instance NFData PutDataCatalogEncryptionSettingsResponse
