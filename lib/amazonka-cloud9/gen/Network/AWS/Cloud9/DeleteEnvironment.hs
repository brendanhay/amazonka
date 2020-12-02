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
-- Module      : Network.AWS.Cloud9.DeleteEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Cloud9 development environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.
--
--
module Network.AWS.Cloud9.DeleteEnvironment
    (
    -- * Creating a Request
      deleteEnvironment
    , DeleteEnvironment
    -- * Request Lenses
    , deEnvironmentId

    -- * Destructuring the Response
    , deleteEnvironmentResponse
    , DeleteEnvironmentResponse
    -- * Response Lenses
    , dersResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEnvironment' smart constructor.
newtype DeleteEnvironment = DeleteEnvironment'
  { _deEnvironmentId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEnvironmentId' - The ID of the environment to delete.
deleteEnvironment
    :: Text -- ^ 'deEnvironmentId'
    -> DeleteEnvironment
deleteEnvironment pEnvironmentId_ =
  DeleteEnvironment' {_deEnvironmentId = pEnvironmentId_}


-- | The ID of the environment to delete.
deEnvironmentId :: Lens' DeleteEnvironment Text
deEnvironmentId = lens _deEnvironmentId (\ s a -> s{_deEnvironmentId = a})

instance AWSRequest DeleteEnvironment where
        type Rs DeleteEnvironment = DeleteEnvironmentResponse
        request = postJSON cloud9
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteEnvironmentResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteEnvironment where

instance NFData DeleteEnvironment where

instance ToHeaders DeleteEnvironment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.DeleteEnvironment"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEnvironment where
        toJSON DeleteEnvironment'{..}
          = object
              (catMaybes
                 [Just ("environmentId" .= _deEnvironmentId)])

instance ToPath DeleteEnvironment where
        toPath = const "/"

instance ToQuery DeleteEnvironment where
        toQuery = const mempty

-- | /See:/ 'deleteEnvironmentResponse' smart constructor.
newtype DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  { _dersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEnvironmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersResponseStatus' - -- | The response status code.
deleteEnvironmentResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DeleteEnvironmentResponse
deleteEnvironmentResponse pResponseStatus_ =
  DeleteEnvironmentResponse' {_dersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dersResponseStatus :: Lens' DeleteEnvironmentResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DeleteEnvironmentResponse where
