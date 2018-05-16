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
-- Module      : Network.AWS.WorkSpaces.UpdateRulesOfIPGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current rules of the specified IP access control group with the specified rules.
--
--
module Network.AWS.WorkSpaces.UpdateRulesOfIPGroup
    (
    -- * Creating a Request
      updateRulesOfIPGroup
    , UpdateRulesOfIPGroup
    -- * Request Lenses
    , uroigGroupId
    , uroigUserRules

    -- * Destructuring the Response
    , updateRulesOfIPGroupResponse
    , UpdateRulesOfIPGroupResponse
    -- * Response Lenses
    , uroigrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'updateRulesOfIPGroup' smart constructor.
data UpdateRulesOfIPGroup = UpdateRulesOfIPGroup'
  { _uroigGroupId   :: !Text
  , _uroigUserRules :: ![IPRuleItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRulesOfIPGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uroigGroupId' - The ID of the group.
--
-- * 'uroigUserRules' - One or more rules.
updateRulesOfIPGroup
    :: Text -- ^ 'uroigGroupId'
    -> UpdateRulesOfIPGroup
updateRulesOfIPGroup pGroupId_ =
  UpdateRulesOfIPGroup' {_uroigGroupId = pGroupId_, _uroigUserRules = mempty}


-- | The ID of the group.
uroigGroupId :: Lens' UpdateRulesOfIPGroup Text
uroigGroupId = lens _uroigGroupId (\ s a -> s{_uroigGroupId = a})

-- | One or more rules.
uroigUserRules :: Lens' UpdateRulesOfIPGroup [IPRuleItem]
uroigUserRules = lens _uroigUserRules (\ s a -> s{_uroigUserRules = a}) . _Coerce

instance AWSRequest UpdateRulesOfIPGroup where
        type Rs UpdateRulesOfIPGroup =
             UpdateRulesOfIPGroupResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateRulesOfIPGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateRulesOfIPGroup where

instance NFData UpdateRulesOfIPGroup where

instance ToHeaders UpdateRulesOfIPGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.UpdateRulesOfIpGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRulesOfIPGroup where
        toJSON UpdateRulesOfIPGroup'{..}
          = object
              (catMaybes
                 [Just ("GroupId" .= _uroigGroupId),
                  Just ("UserRules" .= _uroigUserRules)])

instance ToPath UpdateRulesOfIPGroup where
        toPath = const "/"

instance ToQuery UpdateRulesOfIPGroup where
        toQuery = const mempty

-- | /See:/ 'updateRulesOfIPGroupResponse' smart constructor.
newtype UpdateRulesOfIPGroupResponse = UpdateRulesOfIPGroupResponse'
  { _uroigrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRulesOfIPGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uroigrsResponseStatus' - -- | The response status code.
updateRulesOfIPGroupResponse
    :: Int -- ^ 'uroigrsResponseStatus'
    -> UpdateRulesOfIPGroupResponse
updateRulesOfIPGroupResponse pResponseStatus_ =
  UpdateRulesOfIPGroupResponse' {_uroigrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uroigrsResponseStatus :: Lens' UpdateRulesOfIPGroupResponse Int
uroigrsResponseStatus = lens _uroigrsResponseStatus (\ s a -> s{_uroigrsResponseStatus = a})

instance NFData UpdateRulesOfIPGroupResponse where
