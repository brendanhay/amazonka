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
-- Module      : Network.AWS.Glue.BatchDeleteConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of connection definitions from the Data Catalog.
--
--
module Network.AWS.Glue.BatchDeleteConnection
    (
    -- * Creating a Request
      batchDeleteConnection
    , BatchDeleteConnection
    -- * Request Lenses
    , bdcCatalogId
    , bdcConnectionNameList

    -- * Destructuring the Response
    , batchDeleteConnectionResponse
    , BatchDeleteConnectionResponse
    -- * Response Lenses
    , bdcrsSucceeded
    , bdcrsErrors
    , bdcrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteConnection' smart constructor.
data BatchDeleteConnection = BatchDeleteConnection'
  { _bdcCatalogId          :: !(Maybe Text)
  , _bdcConnectionNameList :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdcCatalogId' - The ID of the Data Catalog in which the connections reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'bdcConnectionNameList' - A list of names of the connections to delete.
batchDeleteConnection
    :: BatchDeleteConnection
batchDeleteConnection =
  BatchDeleteConnection'
    {_bdcCatalogId = Nothing, _bdcConnectionNameList = mempty}


-- | The ID of the Data Catalog in which the connections reside. If none is supplied, the AWS account ID is used by default.
bdcCatalogId :: Lens' BatchDeleteConnection (Maybe Text)
bdcCatalogId = lens _bdcCatalogId (\ s a -> s{_bdcCatalogId = a})

-- | A list of names of the connections to delete.
bdcConnectionNameList :: Lens' BatchDeleteConnection [Text]
bdcConnectionNameList = lens _bdcConnectionNameList (\ s a -> s{_bdcConnectionNameList = a}) . _Coerce

instance AWSRequest BatchDeleteConnection where
        type Rs BatchDeleteConnection =
             BatchDeleteConnectionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteConnectionResponse' <$>
                   (x .?> "Succeeded" .!@ mempty) <*>
                     (x .?> "Errors" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchDeleteConnection where

instance NFData BatchDeleteConnection where

instance ToHeaders BatchDeleteConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchDeleteConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeleteConnection where
        toJSON BatchDeleteConnection'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _bdcCatalogId,
                  Just
                    ("ConnectionNameList" .= _bdcConnectionNameList)])

instance ToPath BatchDeleteConnection where
        toPath = const "/"

instance ToQuery BatchDeleteConnection where
        toQuery = const mempty

-- | /See:/ 'batchDeleteConnectionResponse' smart constructor.
data BatchDeleteConnectionResponse = BatchDeleteConnectionResponse'
  { _bdcrsSucceeded      :: !(Maybe [Text])
  , _bdcrsErrors         :: !(Maybe (Map Text ErrorDetail))
  , _bdcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdcrsSucceeded' - A list of names of the connection definitions that were successfully deleted.
--
-- * 'bdcrsErrors' - A map of the names of connections that were not successfully deleted to error details.
--
-- * 'bdcrsResponseStatus' - -- | The response status code.
batchDeleteConnectionResponse
    :: Int -- ^ 'bdcrsResponseStatus'
    -> BatchDeleteConnectionResponse
batchDeleteConnectionResponse pResponseStatus_ =
  BatchDeleteConnectionResponse'
    { _bdcrsSucceeded = Nothing
    , _bdcrsErrors = Nothing
    , _bdcrsResponseStatus = pResponseStatus_
    }


-- | A list of names of the connection definitions that were successfully deleted.
bdcrsSucceeded :: Lens' BatchDeleteConnectionResponse [Text]
bdcrsSucceeded = lens _bdcrsSucceeded (\ s a -> s{_bdcrsSucceeded = a}) . _Default . _Coerce

-- | A map of the names of connections that were not successfully deleted to error details.
bdcrsErrors :: Lens' BatchDeleteConnectionResponse (HashMap Text ErrorDetail)
bdcrsErrors = lens _bdcrsErrors (\ s a -> s{_bdcrsErrors = a}) . _Default . _Map

-- | -- | The response status code.
bdcrsResponseStatus :: Lens' BatchDeleteConnectionResponse Int
bdcrsResponseStatus = lens _bdcrsResponseStatus (\ s a -> s{_bdcrsResponseStatus = a})

instance NFData BatchDeleteConnectionResponse where
