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
-- Module      : Network.AWS.Glue.DeleteConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection from the Data Catalog.
--
--
module Network.AWS.Glue.DeleteConnection
    (
    -- * Creating a Request
      deleteConnection
    , DeleteConnection
    -- * Request Lenses
    , dcCatalogId
    , dcConnectionName

    -- * Destructuring the Response
    , deleteConnectionResponse
    , DeleteConnectionResponse
    -- * Response Lenses
    , dcrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { _dcCatalogId      :: !(Maybe Text)
  , _dcConnectionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCatalogId' - The ID of the Data Catalog in which the connection resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'dcConnectionName' - The name of the connection to delete.
deleteConnection
    :: Text -- ^ 'dcConnectionName'
    -> DeleteConnection
deleteConnection pConnectionName_ =
  DeleteConnection'
    {_dcCatalogId = Nothing, _dcConnectionName = pConnectionName_}


-- | The ID of the Data Catalog in which the connection resides. If none is supplied, the AWS account ID is used by default.
dcCatalogId :: Lens' DeleteConnection (Maybe Text)
dcCatalogId = lens _dcCatalogId (\ s a -> s{_dcCatalogId = a})

-- | The name of the connection to delete.
dcConnectionName :: Lens' DeleteConnection Text
dcConnectionName = lens _dcConnectionName (\ s a -> s{_dcConnectionName = a})

instance AWSRequest DeleteConnection where
        type Rs DeleteConnection = DeleteConnectionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteConnectionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteConnection where

instance NFData DeleteConnection where

instance ToHeaders DeleteConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteConnection where
        toJSON DeleteConnection'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _dcCatalogId,
                  Just ("ConnectionName" .= _dcConnectionName)])

instance ToPath DeleteConnection where
        toPath = const "/"

instance ToQuery DeleteConnection where
        toQuery = const mempty

-- | /See:/ 'deleteConnectionResponse' smart constructor.
newtype DeleteConnectionResponse = DeleteConnectionResponse'
  { _dcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsResponseStatus' - -- | The response status code.
deleteConnectionResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DeleteConnectionResponse
deleteConnectionResponse pResponseStatus_ =
  DeleteConnectionResponse' {_dcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeleteConnectionResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DeleteConnectionResponse where
