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
-- Module      : Network.AWS.WorkMail.AssociateMemberToGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member to the group's set.
--
--
module Network.AWS.WorkMail.AssociateMemberToGroup
    (
    -- * Creating a Request
      associateMemberToGroup
    , AssociateMemberToGroup
    -- * Request Lenses
    , amtgOrganizationId
    , amtgGroupId
    , amtgMemberId

    -- * Destructuring the Response
    , associateMemberToGroupResponse
    , AssociateMemberToGroupResponse
    -- * Response Lenses
    , amtgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'associateMemberToGroup' smart constructor.
data AssociateMemberToGroup = AssociateMemberToGroup'
  { _amtgOrganizationId :: !Text
  , _amtgGroupId        :: !Text
  , _amtgMemberId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateMemberToGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amtgOrganizationId' - The organization under which the group exists.
--
-- * 'amtgGroupId' - The group for which the member is associated.
--
-- * 'amtgMemberId' - The member to associate to the group.
associateMemberToGroup
    :: Text -- ^ 'amtgOrganizationId'
    -> Text -- ^ 'amtgGroupId'
    -> Text -- ^ 'amtgMemberId'
    -> AssociateMemberToGroup
associateMemberToGroup pOrganizationId_ pGroupId_ pMemberId_ =
  AssociateMemberToGroup'
    { _amtgOrganizationId = pOrganizationId_
    , _amtgGroupId = pGroupId_
    , _amtgMemberId = pMemberId_
    }


-- | The organization under which the group exists.
amtgOrganizationId :: Lens' AssociateMemberToGroup Text
amtgOrganizationId = lens _amtgOrganizationId (\ s a -> s{_amtgOrganizationId = a})

-- | The group for which the member is associated.
amtgGroupId :: Lens' AssociateMemberToGroup Text
amtgGroupId = lens _amtgGroupId (\ s a -> s{_amtgGroupId = a})

-- | The member to associate to the group.
amtgMemberId :: Lens' AssociateMemberToGroup Text
amtgMemberId = lens _amtgMemberId (\ s a -> s{_amtgMemberId = a})

instance AWSRequest AssociateMemberToGroup where
        type Rs AssociateMemberToGroup =
             AssociateMemberToGroupResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateMemberToGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateMemberToGroup where

instance NFData AssociateMemberToGroup where

instance ToHeaders AssociateMemberToGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.AssociateMemberToGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateMemberToGroup where
        toJSON AssociateMemberToGroup'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _amtgOrganizationId),
                  Just ("GroupId" .= _amtgGroupId),
                  Just ("MemberId" .= _amtgMemberId)])

instance ToPath AssociateMemberToGroup where
        toPath = const "/"

instance ToQuery AssociateMemberToGroup where
        toQuery = const mempty

-- | /See:/ 'associateMemberToGroupResponse' smart constructor.
newtype AssociateMemberToGroupResponse = AssociateMemberToGroupResponse'
  { _amtgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateMemberToGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amtgrsResponseStatus' - -- | The response status code.
associateMemberToGroupResponse
    :: Int -- ^ 'amtgrsResponseStatus'
    -> AssociateMemberToGroupResponse
associateMemberToGroupResponse pResponseStatus_ =
  AssociateMemberToGroupResponse' {_amtgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
amtgrsResponseStatus :: Lens' AssociateMemberToGroupResponse Int
amtgrsResponseStatus = lens _amtgrsResponseStatus (\ s a -> s{_amtgrsResponseStatus = a})

instance NFData AssociateMemberToGroupResponse where
