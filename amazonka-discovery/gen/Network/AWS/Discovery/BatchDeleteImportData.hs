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
-- Module      : Network.AWS.Discovery.BatchDeleteImportData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more import tasks, each identified by their import ID. Each import task has a number of records that can identify servers or applications.
--
--
-- AWS Application Discovery Service has built-in matching logic that will identify when discovered servers match existing entries that you've previously discovered, the information for the already-existing discovered server is updated. When you delete an import task that contains records that were used to match, the information in those matched records that comes from the deleted records will also be deleted.
--
module Network.AWS.Discovery.BatchDeleteImportData
    (
    -- * Creating a Request
      batchDeleteImportData
    , BatchDeleteImportData
    -- * Request Lenses
    , bdidImportTaskIds

    -- * Destructuring the Response
    , batchDeleteImportDataResponse
    , BatchDeleteImportDataResponse
    -- * Response Lenses
    , bdidrsErrors
    , bdidrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteImportData' smart constructor.
newtype BatchDeleteImportData = BatchDeleteImportData'
  { _bdidImportTaskIds :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteImportData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdidImportTaskIds' - The IDs for the import tasks that you want to delete.
batchDeleteImportData
    :: NonEmpty Text -- ^ 'bdidImportTaskIds'
    -> BatchDeleteImportData
batchDeleteImportData pImportTaskIds_ =
  BatchDeleteImportData' {_bdidImportTaskIds = _List1 # pImportTaskIds_}


-- | The IDs for the import tasks that you want to delete.
bdidImportTaskIds :: Lens' BatchDeleteImportData (NonEmpty Text)
bdidImportTaskIds = lens _bdidImportTaskIds (\ s a -> s{_bdidImportTaskIds = a}) . _List1

instance AWSRequest BatchDeleteImportData where
        type Rs BatchDeleteImportData =
             BatchDeleteImportDataResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteImportDataResponse' <$>
                   (x .?> "errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchDeleteImportData where

instance NFData BatchDeleteImportData where

instance ToHeaders BatchDeleteImportData where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.BatchDeleteImportData"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeleteImportData where
        toJSON BatchDeleteImportData'{..}
          = object
              (catMaybes
                 [Just ("importTaskIds" .= _bdidImportTaskIds)])

instance ToPath BatchDeleteImportData where
        toPath = const "/"

instance ToQuery BatchDeleteImportData where
        toQuery = const mempty

-- | /See:/ 'batchDeleteImportDataResponse' smart constructor.
data BatchDeleteImportDataResponse = BatchDeleteImportDataResponse'
  { _bdidrsErrors         :: !(Maybe [BatchDeleteImportDataError])
  , _bdidrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteImportDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdidrsErrors' - Error messages returned for each import task that you deleted as a response for this command.
--
-- * 'bdidrsResponseStatus' - -- | The response status code.
batchDeleteImportDataResponse
    :: Int -- ^ 'bdidrsResponseStatus'
    -> BatchDeleteImportDataResponse
batchDeleteImportDataResponse pResponseStatus_ =
  BatchDeleteImportDataResponse'
    {_bdidrsErrors = Nothing, _bdidrsResponseStatus = pResponseStatus_}


-- | Error messages returned for each import task that you deleted as a response for this command.
bdidrsErrors :: Lens' BatchDeleteImportDataResponse [BatchDeleteImportDataError]
bdidrsErrors = lens _bdidrsErrors (\ s a -> s{_bdidrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdidrsResponseStatus :: Lens' BatchDeleteImportDataResponse Int
bdidrsResponseStatus = lens _bdidrsResponseStatus (\ s a -> s{_bdidrsResponseStatus = a})

instance NFData BatchDeleteImportDataResponse where
