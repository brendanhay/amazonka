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
-- Module      : Network.AWS.IoTAnalytics.DeleteDatasetContent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of the specified data set.
--
--
module Network.AWS.IoTAnalytics.DeleteDatasetContent
    (
    -- * Creating a Request
      deleteDatasetContent
    , DeleteDatasetContent
    -- * Request Lenses
    , ddcVersionId
    , ddcDatasetName

    -- * Destructuring the Response
    , deleteDatasetContentResponse
    , DeleteDatasetContentResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDatasetContent' smart constructor.
data DeleteDatasetContent = DeleteDatasetContent'
  { _ddcVersionId   :: !(Maybe Text)
  , _ddcDatasetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatasetContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcVersionId' - The version of the data set whose content is deleted. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to delete the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- * 'ddcDatasetName' - The name of the data set whose content is deleted.
deleteDatasetContent
    :: Text -- ^ 'ddcDatasetName'
    -> DeleteDatasetContent
deleteDatasetContent pDatasetName_ =
  DeleteDatasetContent'
    {_ddcVersionId = Nothing, _ddcDatasetName = pDatasetName_}


-- | The version of the data set whose content is deleted. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to delete the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
ddcVersionId :: Lens' DeleteDatasetContent (Maybe Text)
ddcVersionId = lens _ddcVersionId (\ s a -> s{_ddcVersionId = a})

-- | The name of the data set whose content is deleted.
ddcDatasetName :: Lens' DeleteDatasetContent Text
ddcDatasetName = lens _ddcDatasetName (\ s a -> s{_ddcDatasetName = a})

instance AWSRequest DeleteDatasetContent where
        type Rs DeleteDatasetContent =
             DeleteDatasetContentResponse
        request = delete ioTAnalytics
        response = receiveNull DeleteDatasetContentResponse'

instance Hashable DeleteDatasetContent where

instance NFData DeleteDatasetContent where

instance ToHeaders DeleteDatasetContent where
        toHeaders = const mempty

instance ToPath DeleteDatasetContent where
        toPath DeleteDatasetContent'{..}
          = mconcat
              ["/datasets/", toBS _ddcDatasetName, "/content"]

instance ToQuery DeleteDatasetContent where
        toQuery DeleteDatasetContent'{..}
          = mconcat ["versionId" =: _ddcVersionId]

-- | /See:/ 'deleteDatasetContentResponse' smart constructor.
data DeleteDatasetContentResponse =
  DeleteDatasetContentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatasetContentResponse' with the minimum fields required to make a request.
--
deleteDatasetContentResponse
    :: DeleteDatasetContentResponse
deleteDatasetContentResponse = DeleteDatasetContentResponse'


instance NFData DeleteDatasetContentResponse where
