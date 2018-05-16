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
-- Module      : Network.AWS.Cloud9.DeleteEnvironmentMembership
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment member from an AWS Cloud9 development environment.
--
--
module Network.AWS.Cloud9.DeleteEnvironmentMembership
    (
    -- * Creating a Request
      deleteEnvironmentMembership
    , DeleteEnvironmentMembership
    -- * Request Lenses
    , demEnvironmentId
    , demUserARN

    -- * Destructuring the Response
    , deleteEnvironmentMembershipResponse
    , DeleteEnvironmentMembershipResponse
    -- * Response Lenses
    , demrsResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEnvironmentMembership' smart constructor.
data DeleteEnvironmentMembership = DeleteEnvironmentMembership'
  { _demEnvironmentId :: !Text
  , _demUserARN       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEnvironmentMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demEnvironmentId' - The ID of the environment to delete the environment member from.
--
-- * 'demUserARN' - The Amazon Resource Name (ARN) of the environment member to delete from the environment.
deleteEnvironmentMembership
    :: Text -- ^ 'demEnvironmentId'
    -> Text -- ^ 'demUserARN'
    -> DeleteEnvironmentMembership
deleteEnvironmentMembership pEnvironmentId_ pUserARN_ =
  DeleteEnvironmentMembership'
    {_demEnvironmentId = pEnvironmentId_, _demUserARN = pUserARN_}


-- | The ID of the environment to delete the environment member from.
demEnvironmentId :: Lens' DeleteEnvironmentMembership Text
demEnvironmentId = lens _demEnvironmentId (\ s a -> s{_demEnvironmentId = a})

-- | The Amazon Resource Name (ARN) of the environment member to delete from the environment.
demUserARN :: Lens' DeleteEnvironmentMembership Text
demUserARN = lens _demUserARN (\ s a -> s{_demUserARN = a})

instance AWSRequest DeleteEnvironmentMembership where
        type Rs DeleteEnvironmentMembership =
             DeleteEnvironmentMembershipResponse
        request = postJSON cloud9
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteEnvironmentMembershipResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteEnvironmentMembership where

instance NFData DeleteEnvironmentMembership where

instance ToHeaders DeleteEnvironmentMembership where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.DeleteEnvironmentMembership"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEnvironmentMembership where
        toJSON DeleteEnvironmentMembership'{..}
          = object
              (catMaybes
                 [Just ("environmentId" .= _demEnvironmentId),
                  Just ("userArn" .= _demUserARN)])

instance ToPath DeleteEnvironmentMembership where
        toPath = const "/"

instance ToQuery DeleteEnvironmentMembership where
        toQuery = const mempty

-- | /See:/ 'deleteEnvironmentMembershipResponse' smart constructor.
newtype DeleteEnvironmentMembershipResponse = DeleteEnvironmentMembershipResponse'
  { _demrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEnvironmentMembershipResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demrsResponseStatus' - -- | The response status code.
deleteEnvironmentMembershipResponse
    :: Int -- ^ 'demrsResponseStatus'
    -> DeleteEnvironmentMembershipResponse
deleteEnvironmentMembershipResponse pResponseStatus_ =
  DeleteEnvironmentMembershipResponse' {_demrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
demrsResponseStatus :: Lens' DeleteEnvironmentMembershipResponse Int
demrsResponseStatus = lens _demrsResponseStatus (\ s a -> s{_demrsResponseStatus = a})

instance NFData DeleteEnvironmentMembershipResponse
         where
