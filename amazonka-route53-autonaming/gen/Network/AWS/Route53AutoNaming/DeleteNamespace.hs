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
-- Module      : Network.AWS.Route53AutoNaming.DeleteNamespace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace from the current account. If the namespace still contains one or more services, the request fails.
--
--
module Network.AWS.Route53AutoNaming.DeleteNamespace
    (
    -- * Creating a Request
      deleteNamespace
    , DeleteNamespace
    -- * Request Lenses
    , dnId

    -- * Destructuring the Response
    , deleteNamespaceResponse
    , DeleteNamespaceResponse
    -- * Response Lenses
    , dnrsOperationId
    , dnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'deleteNamespace' smart constructor.
newtype DeleteNamespace = DeleteNamespace'
  { _dnId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNamespace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnId' - The ID of the namespace that you want to delete.
deleteNamespace
    :: Text -- ^ 'dnId'
    -> DeleteNamespace
deleteNamespace pId_ = DeleteNamespace' {_dnId = pId_}


-- | The ID of the namespace that you want to delete.
dnId :: Lens' DeleteNamespace Text
dnId = lens _dnId (\ s a -> s{_dnId = a})

instance AWSRequest DeleteNamespace where
        type Rs DeleteNamespace = DeleteNamespaceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 DeleteNamespaceResponse' <$>
                   (x .?> "OperationId") <*> (pure (fromEnum s)))

instance Hashable DeleteNamespace where

instance NFData DeleteNamespace where

instance ToHeaders DeleteNamespace where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.DeleteNamespace" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteNamespace where
        toJSON DeleteNamespace'{..}
          = object (catMaybes [Just ("Id" .= _dnId)])

instance ToPath DeleteNamespace where
        toPath = const "/"

instance ToQuery DeleteNamespace where
        toQuery = const mempty

-- | /See:/ 'deleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { _dnrsOperationId    :: !(Maybe Text)
  , _dnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNamespaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnrsOperationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
--
-- * 'dnrsResponseStatus' - -- | The response status code.
deleteNamespaceResponse
    :: Int -- ^ 'dnrsResponseStatus'
    -> DeleteNamespaceResponse
deleteNamespaceResponse pResponseStatus_ =
  DeleteNamespaceResponse'
    {_dnrsOperationId = Nothing, _dnrsResponseStatus = pResponseStatus_}


-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
dnrsOperationId :: Lens' DeleteNamespaceResponse (Maybe Text)
dnrsOperationId = lens _dnrsOperationId (\ s a -> s{_dnrsOperationId = a})

-- | -- | The response status code.
dnrsResponseStatus :: Lens' DeleteNamespaceResponse Int
dnrsResponseStatus = lens _dnrsResponseStatus (\ s a -> s{_dnrsResponseStatus = a})

instance NFData DeleteNamespaceResponse where
