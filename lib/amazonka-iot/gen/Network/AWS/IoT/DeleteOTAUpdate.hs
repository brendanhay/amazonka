{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteOTAUpdate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an OTA update.
--
--
module Network.AWS.IoT.DeleteOTAUpdate
    (
    -- * Creating a Request
      deleteOTAUpdate
    , DeleteOTAUpdate
    -- * Request Lenses
    , dotauOtaUpdateId

    -- * Destructuring the Response
    , deleteOTAUpdateResponse
    , DeleteOTAUpdateResponse
    -- * Response Lenses
    , dotaursResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOTAUpdate' smart constructor.
newtype DeleteOTAUpdate = DeleteOTAUpdate'
  { _dotauOtaUpdateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOTAUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotauOtaUpdateId' - The OTA update ID to delete.
deleteOTAUpdate
    :: Text -- ^ 'dotauOtaUpdateId'
    -> DeleteOTAUpdate
deleteOTAUpdate pOtaUpdateId_ =
  DeleteOTAUpdate' {_dotauOtaUpdateId = pOtaUpdateId_}


-- | The OTA update ID to delete.
dotauOtaUpdateId :: Lens' DeleteOTAUpdate Text
dotauOtaUpdateId = lens _dotauOtaUpdateId (\ s a -> s{_dotauOtaUpdateId = a})

instance AWSRequest DeleteOTAUpdate where
        type Rs DeleteOTAUpdate = DeleteOTAUpdateResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteOTAUpdateResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteOTAUpdate where

instance NFData DeleteOTAUpdate where

instance ToHeaders DeleteOTAUpdate where
        toHeaders = const mempty

instance ToPath DeleteOTAUpdate where
        toPath DeleteOTAUpdate'{..}
          = mconcat ["/otaUpdates/", toBS _dotauOtaUpdateId]

instance ToQuery DeleteOTAUpdate where
        toQuery = const mempty

-- | /See:/ 'deleteOTAUpdateResponse' smart constructor.
newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { _dotaursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOTAUpdateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotaursResponseStatus' - -- | The response status code.
deleteOTAUpdateResponse
    :: Int -- ^ 'dotaursResponseStatus'
    -> DeleteOTAUpdateResponse
deleteOTAUpdateResponse pResponseStatus_ =
  DeleteOTAUpdateResponse' {_dotaursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dotaursResponseStatus :: Lens' DeleteOTAUpdateResponse Int
dotaursResponseStatus = lens _dotaursResponseStatus (\ s a -> s{_dotaursResponseStatus = a})

instance NFData DeleteOTAUpdateResponse where
