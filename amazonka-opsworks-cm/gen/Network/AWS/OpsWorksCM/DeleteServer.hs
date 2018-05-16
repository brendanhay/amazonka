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
-- Module      : Network.AWS.OpsWorksCM.DeleteServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the server and the underlying AWS CloudFormation stacks (including the server's EC2 instance). When you run this command, the server state is updated to @DELETING@ . After the server is deleted, it is no longer returned by @DescribeServer@ requests. If the AWS CloudFormation stack cannot be deleted, the server cannot be deleted.
--
--
-- This operation is asynchronous.
--
-- An @InvalidStateException@ is thrown when a server deletion is already in progress. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
--
--
module Network.AWS.OpsWorksCM.DeleteServer
    (
    -- * Creating a Request
      deleteServer
    , DeleteServer
    -- * Request Lenses
    , dsServerName

    -- * Destructuring the Response
    , deleteServerResponse
    , DeleteServerResponse
    -- * Response Lenses
    , dsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteServer' smart constructor.
newtype DeleteServer = DeleteServer'
  { _dsServerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsServerName' - The ID of the server to delete.
deleteServer
    :: Text -- ^ 'dsServerName'
    -> DeleteServer
deleteServer pServerName_ = DeleteServer' {_dsServerName = pServerName_}


-- | The ID of the server to delete.
dsServerName :: Lens' DeleteServer Text
dsServerName = lens _dsServerName (\ s a -> s{_dsServerName = a})

instance AWSRequest DeleteServer where
        type Rs DeleteServer = DeleteServerResponse
        request = postJSON opsWorksCM
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteServerResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteServer where

instance NFData DeleteServer where

instance ToHeaders DeleteServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.DeleteServer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteServer where
        toJSON DeleteServer'{..}
          = object
              (catMaybes [Just ("ServerName" .= _dsServerName)])

instance ToPath DeleteServer where
        toPath = const "/"

instance ToQuery DeleteServer where
        toQuery = const mempty

-- | /See:/ 'deleteServerResponse' smart constructor.
newtype DeleteServerResponse = DeleteServerResponse'
  { _dsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteServerResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteServerResponse
deleteServerResponse pResponseStatus_ =
  DeleteServerResponse' {_dsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteServerResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteServerResponse where
