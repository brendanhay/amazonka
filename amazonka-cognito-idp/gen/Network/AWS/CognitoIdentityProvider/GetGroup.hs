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
-- Module      : Network.AWS.CognitoIdentityProvider.GetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a group.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.GetGroup
    (
    -- * Creating a Request
      getGroup
    , GetGroup
    -- * Request Lenses
    , ggGroupName
    , ggUserPoolId

    -- * Destructuring the Response
    , getGroupResponse
    , GetGroupResponse
    -- * Response Lenses
    , ggrsGroup
    , ggrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGroup' smart constructor.
data GetGroup = GetGroup'
  { _ggGroupName  :: !Text
  , _ggUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggGroupName' - The name of the group.
--
-- * 'ggUserPoolId' - The user pool ID for the user pool.
getGroup
    :: Text -- ^ 'ggGroupName'
    -> Text -- ^ 'ggUserPoolId'
    -> GetGroup
getGroup pGroupName_ pUserPoolId_ =
  GetGroup' {_ggGroupName = pGroupName_, _ggUserPoolId = pUserPoolId_}


-- | The name of the group.
ggGroupName :: Lens' GetGroup Text
ggGroupName = lens _ggGroupName (\ s a -> s{_ggGroupName = a})

-- | The user pool ID for the user pool.
ggUserPoolId :: Lens' GetGroup Text
ggUserPoolId = lens _ggUserPoolId (\ s a -> s{_ggUserPoolId = a})

instance AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable GetGroup where

instance NFData GetGroup where

instance ToHeaders GetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetGroup where
        toJSON GetGroup'{..}
          = object
              (catMaybes
                 [Just ("GroupName" .= _ggGroupName),
                  Just ("UserPoolId" .= _ggUserPoolId)])

instance ToPath GetGroup where
        toPath = const "/"

instance ToQuery GetGroup where
        toQuery = const mempty

-- | /See:/ 'getGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { _ggrsGroup          :: !(Maybe GroupType)
  , _ggrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsGroup' - The group object for the group.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getGroupResponse
    :: Int -- ^ 'ggrsResponseStatus'
    -> GetGroupResponse
getGroupResponse pResponseStatus_ =
  GetGroupResponse'
    {_ggrsGroup = Nothing, _ggrsResponseStatus = pResponseStatus_}


-- | The group object for the group.
ggrsGroup :: Lens' GetGroupResponse (Maybe GroupType)
ggrsGroup = lens _ggrsGroup (\ s a -> s{_ggrsGroup = a})

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetGroupResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a})

instance NFData GetGroupResponse where
