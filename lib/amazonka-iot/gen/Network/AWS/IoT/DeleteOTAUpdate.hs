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
-- Module      : Network.AWS.IoT.DeleteOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an OTA update.
module Network.AWS.IoT.DeleteOTAUpdate
  ( -- * Creating a Request
    deleteOTAUpdate,
    DeleteOTAUpdate,

    -- * Request Lenses
    dotauForceDeleteAWSJob,
    dotauDeleteStream,
    dotauOtaUpdateId,

    -- * Destructuring the Response
    deleteOTAUpdateResponse,
    DeleteOTAUpdateResponse,

    -- * Response Lenses
    dotaursResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOTAUpdate' smart constructor.
data DeleteOTAUpdate = DeleteOTAUpdate'
  { _dotauForceDeleteAWSJob ::
      !(Maybe Bool),
    _dotauDeleteStream :: !(Maybe Bool),
    _dotauOtaUpdateId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOTAUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotauForceDeleteAWSJob' - Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
--
-- * 'dotauDeleteStream' - Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
--
-- * 'dotauOtaUpdateId' - The ID of the OTA update to delete.
deleteOTAUpdate ::
  -- | 'dotauOtaUpdateId'
  Text ->
  DeleteOTAUpdate
deleteOTAUpdate pOtaUpdateId_ =
  DeleteOTAUpdate'
    { _dotauForceDeleteAWSJob = Nothing,
      _dotauDeleteStream = Nothing,
      _dotauOtaUpdateId = pOtaUpdateId_
    }

-- | Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
dotauForceDeleteAWSJob :: Lens' DeleteOTAUpdate (Maybe Bool)
dotauForceDeleteAWSJob = lens _dotauForceDeleteAWSJob (\s a -> s {_dotauForceDeleteAWSJob = a})

-- | Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
dotauDeleteStream :: Lens' DeleteOTAUpdate (Maybe Bool)
dotauDeleteStream = lens _dotauDeleteStream (\s a -> s {_dotauDeleteStream = a})

-- | The ID of the OTA update to delete.
dotauOtaUpdateId :: Lens' DeleteOTAUpdate Text
dotauOtaUpdateId = lens _dotauOtaUpdateId (\s a -> s {_dotauOtaUpdateId = a})

instance AWSRequest DeleteOTAUpdate where
  type Rs DeleteOTAUpdate = DeleteOTAUpdateResponse
  request = delete ioT
  response =
    receiveEmpty
      (\s h x -> DeleteOTAUpdateResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteOTAUpdate

instance NFData DeleteOTAUpdate

instance ToHeaders DeleteOTAUpdate where
  toHeaders = const mempty

instance ToPath DeleteOTAUpdate where
  toPath DeleteOTAUpdate' {..} =
    mconcat ["/otaUpdates/", toBS _dotauOtaUpdateId]

instance ToQuery DeleteOTAUpdate where
  toQuery DeleteOTAUpdate' {..} =
    mconcat
      [ "forceDeleteAWSJob" =: _dotauForceDeleteAWSJob,
        "deleteStream" =: _dotauDeleteStream
      ]

-- | /See:/ 'deleteOTAUpdateResponse' smart constructor.
newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { _dotaursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOTAUpdateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotaursResponseStatus' - -- | The response status code.
deleteOTAUpdateResponse ::
  -- | 'dotaursResponseStatus'
  Int ->
  DeleteOTAUpdateResponse
deleteOTAUpdateResponse pResponseStatus_ =
  DeleteOTAUpdateResponse'
    { _dotaursResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dotaursResponseStatus :: Lens' DeleteOTAUpdateResponse Int
dotaursResponseStatus = lens _dotaursResponseStatus (\s a -> s {_dotaursResponseStatus = a})

instance NFData DeleteOTAUpdateResponse
