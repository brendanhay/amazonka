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
-- Module      : Network.AWS.WorkSpaces.AuthorizeIPRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more rules to the specified IP access control group.
--
--
-- This action gives users permission to access their WorkSpaces from the CIDR address ranges specified in the rules.
--
module Network.AWS.WorkSpaces.AuthorizeIPRules
    (
    -- * Creating a Request
      authorizeIPRules
    , AuthorizeIPRules
    -- * Request Lenses
    , airGroupId
    , airUserRules

    -- * Destructuring the Response
    , authorizeIPRulesResponse
    , AuthorizeIPRulesResponse
    -- * Response Lenses
    , airrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'authorizeIPRules' smart constructor.
data AuthorizeIPRules = AuthorizeIPRules'
  { _airGroupId   :: !Text
  , _airUserRules :: ![IPRuleItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeIPRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airGroupId' - The ID of the group.
--
-- * 'airUserRules' - The rules to add to the group.
authorizeIPRules
    :: Text -- ^ 'airGroupId'
    -> AuthorizeIPRules
authorizeIPRules pGroupId_ =
  AuthorizeIPRules' {_airGroupId = pGroupId_, _airUserRules = mempty}


-- | The ID of the group.
airGroupId :: Lens' AuthorizeIPRules Text
airGroupId = lens _airGroupId (\ s a -> s{_airGroupId = a})

-- | The rules to add to the group.
airUserRules :: Lens' AuthorizeIPRules [IPRuleItem]
airUserRules = lens _airUserRules (\ s a -> s{_airUserRules = a}) . _Coerce

instance AWSRequest AuthorizeIPRules where
        type Rs AuthorizeIPRules = AuthorizeIPRulesResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 AuthorizeIPRulesResponse' <$> (pure (fromEnum s)))

instance Hashable AuthorizeIPRules where

instance NFData AuthorizeIPRules where

instance ToHeaders AuthorizeIPRules where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.AuthorizeIpRules" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AuthorizeIPRules where
        toJSON AuthorizeIPRules'{..}
          = object
              (catMaybes
                 [Just ("GroupId" .= _airGroupId),
                  Just ("UserRules" .= _airUserRules)])

instance ToPath AuthorizeIPRules where
        toPath = const "/"

instance ToQuery AuthorizeIPRules where
        toQuery = const mempty

-- | /See:/ 'authorizeIPRulesResponse' smart constructor.
newtype AuthorizeIPRulesResponse = AuthorizeIPRulesResponse'
  { _airrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizeIPRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airrsResponseStatus' - -- | The response status code.
authorizeIPRulesResponse
    :: Int -- ^ 'airrsResponseStatus'
    -> AuthorizeIPRulesResponse
authorizeIPRulesResponse pResponseStatus_ =
  AuthorizeIPRulesResponse' {_airrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
airrsResponseStatus :: Lens' AuthorizeIPRulesResponse Int
airrsResponseStatus = lens _airrsResponseStatus (\ s a -> s{_airrsResponseStatus = a})

instance NFData AuthorizeIPRulesResponse where
