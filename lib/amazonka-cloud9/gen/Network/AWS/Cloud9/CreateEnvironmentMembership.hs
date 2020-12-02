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
-- Module      : Network.AWS.Cloud9.CreateEnvironmentMembership
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an environment member to an AWS Cloud9 development environment.
--
--
module Network.AWS.Cloud9.CreateEnvironmentMembership
    (
    -- * Creating a Request
      createEnvironmentMembership
    , CreateEnvironmentMembership
    -- * Request Lenses
    , cemEnvironmentId
    , cemUserARN
    , cemPermissions

    -- * Destructuring the Response
    , createEnvironmentMembershipResponse
    , CreateEnvironmentMembershipResponse
    -- * Response Lenses
    , cemrsMembership
    , cemrsResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEnvironmentMembership' smart constructor.
data CreateEnvironmentMembership = CreateEnvironmentMembership'
  { _cemEnvironmentId :: !Text
  , _cemUserARN       :: !Text
  , _cemPermissions   :: !MemberPermissions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEnvironmentMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cemEnvironmentId' - The ID of the environment that contains the environment member you want to add.
--
-- * 'cemUserARN' - The Amazon Resource Name (ARN) of the environment member you want to add.
--
-- * 'cemPermissions' - The type of environment member permissions you want to associate with this environment member. Available values include:     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment.
createEnvironmentMembership
    :: Text -- ^ 'cemEnvironmentId'
    -> Text -- ^ 'cemUserARN'
    -> MemberPermissions -- ^ 'cemPermissions'
    -> CreateEnvironmentMembership
createEnvironmentMembership pEnvironmentId_ pUserARN_ pPermissions_ =
  CreateEnvironmentMembership'
    { _cemEnvironmentId = pEnvironmentId_
    , _cemUserARN = pUserARN_
    , _cemPermissions = pPermissions_
    }


-- | The ID of the environment that contains the environment member you want to add.
cemEnvironmentId :: Lens' CreateEnvironmentMembership Text
cemEnvironmentId = lens _cemEnvironmentId (\ s a -> s{_cemEnvironmentId = a})

-- | The Amazon Resource Name (ARN) of the environment member you want to add.
cemUserARN :: Lens' CreateEnvironmentMembership Text
cemUserARN = lens _cemUserARN (\ s a -> s{_cemUserARN = a})

-- | The type of environment member permissions you want to associate with this environment member. Available values include:     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment.
cemPermissions :: Lens' CreateEnvironmentMembership MemberPermissions
cemPermissions = lens _cemPermissions (\ s a -> s{_cemPermissions = a})

instance AWSRequest CreateEnvironmentMembership where
        type Rs CreateEnvironmentMembership =
             CreateEnvironmentMembershipResponse
        request = postJSON cloud9
        response
          = receiveJSON
              (\ s h x ->
                 CreateEnvironmentMembershipResponse' <$>
                   (x .?> "membership") <*> (pure (fromEnum s)))

instance Hashable CreateEnvironmentMembership where

instance NFData CreateEnvironmentMembership where

instance ToHeaders CreateEnvironmentMembership where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.CreateEnvironmentMembership"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEnvironmentMembership where
        toJSON CreateEnvironmentMembership'{..}
          = object
              (catMaybes
                 [Just ("environmentId" .= _cemEnvironmentId),
                  Just ("userArn" .= _cemUserARN),
                  Just ("permissions" .= _cemPermissions)])

instance ToPath CreateEnvironmentMembership where
        toPath = const "/"

instance ToQuery CreateEnvironmentMembership where
        toQuery = const mempty

-- | /See:/ 'createEnvironmentMembershipResponse' smart constructor.
data CreateEnvironmentMembershipResponse = CreateEnvironmentMembershipResponse'
  { _cemrsMembership     :: !(Maybe EnvironmentMember)
  , _cemrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEnvironmentMembershipResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cemrsMembership' - Information about the environment member that was added.
--
-- * 'cemrsResponseStatus' - -- | The response status code.
createEnvironmentMembershipResponse
    :: Int -- ^ 'cemrsResponseStatus'
    -> CreateEnvironmentMembershipResponse
createEnvironmentMembershipResponse pResponseStatus_ =
  CreateEnvironmentMembershipResponse'
    {_cemrsMembership = Nothing, _cemrsResponseStatus = pResponseStatus_}


-- | Information about the environment member that was added.
cemrsMembership :: Lens' CreateEnvironmentMembershipResponse (Maybe EnvironmentMember)
cemrsMembership = lens _cemrsMembership (\ s a -> s{_cemrsMembership = a})

-- | -- | The response status code.
cemrsResponseStatus :: Lens' CreateEnvironmentMembershipResponse Int
cemrsResponseStatus = lens _cemrsResponseStatus (\ s a -> s{_cemrsResponseStatus = a})

instance NFData CreateEnvironmentMembershipResponse
         where
