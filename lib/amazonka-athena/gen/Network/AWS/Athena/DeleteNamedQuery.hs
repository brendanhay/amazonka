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
-- Module      : Network.AWS.Athena.DeleteNamedQuery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a named query.
--
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
module Network.AWS.Athena.DeleteNamedQuery
    (
    -- * Creating a Request
      deleteNamedQuery
    , DeleteNamedQuery
    -- * Request Lenses
    , dnqNamedQueryId

    -- * Destructuring the Response
    , deleteNamedQueryResponse
    , DeleteNamedQueryResponse
    -- * Response Lenses
    , dnqrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNamedQuery' smart constructor.
newtype DeleteNamedQuery = DeleteNamedQuery'
  { _dnqNamedQueryId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNamedQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnqNamedQueryId' - The unique ID of the query to delete.
deleteNamedQuery
    :: Text -- ^ 'dnqNamedQueryId'
    -> DeleteNamedQuery
deleteNamedQuery pNamedQueryId_ =
  DeleteNamedQuery' {_dnqNamedQueryId = pNamedQueryId_}


-- | The unique ID of the query to delete.
dnqNamedQueryId :: Lens' DeleteNamedQuery Text
dnqNamedQueryId = lens _dnqNamedQueryId (\ s a -> s{_dnqNamedQueryId = a})

instance AWSRequest DeleteNamedQuery where
        type Rs DeleteNamedQuery = DeleteNamedQueryResponse
        request = postJSON athena
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteNamedQueryResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteNamedQuery where

instance NFData DeleteNamedQuery where

instance ToHeaders DeleteNamedQuery where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.DeleteNamedQuery" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteNamedQuery where
        toJSON DeleteNamedQuery'{..}
          = object
              (catMaybes
                 [Just ("NamedQueryId" .= _dnqNamedQueryId)])

instance ToPath DeleteNamedQuery where
        toPath = const "/"

instance ToQuery DeleteNamedQuery where
        toQuery = const mempty

-- | /See:/ 'deleteNamedQueryResponse' smart constructor.
newtype DeleteNamedQueryResponse = DeleteNamedQueryResponse'
  { _dnqrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNamedQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnqrsResponseStatus' - -- | The response status code.
deleteNamedQueryResponse
    :: Int -- ^ 'dnqrsResponseStatus'
    -> DeleteNamedQueryResponse
deleteNamedQueryResponse pResponseStatus_ =
  DeleteNamedQueryResponse' {_dnqrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dnqrsResponseStatus :: Lens' DeleteNamedQueryResponse Int
dnqrsResponseStatus = lens _dnqrsResponseStatus (\ s a -> s{_dnqrsResponseStatus = a})

instance NFData DeleteNamedQueryResponse where
