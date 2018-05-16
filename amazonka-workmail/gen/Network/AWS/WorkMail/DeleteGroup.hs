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
-- Module      : Network.AWS.WorkMail.DeleteGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group from Amazon WorkMail.
--
--
module Network.AWS.WorkMail.DeleteGroup
    (
    -- * Creating a Request
      deleteGroup
    , DeleteGroup
    -- * Request Lenses
    , dggOrganizationId
    , dggGroupId

    -- * Destructuring the Response
    , deleteGroupResponse
    , DeleteGroupResponse
    -- * Response Lenses
    , dgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'deleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { _dggOrganizationId :: !Text
  , _dggGroupId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dggOrganizationId' - The organization that contains the group.
--
-- * 'dggGroupId' - The identifier of the group to be deleted.
deleteGroup
    :: Text -- ^ 'dggOrganizationId'
    -> Text -- ^ 'dggGroupId'
    -> DeleteGroup
deleteGroup pOrganizationId_ pGroupId_ =
  DeleteGroup' {_dggOrganizationId = pOrganizationId_, _dggGroupId = pGroupId_}


-- | The organization that contains the group.
dggOrganizationId :: Lens' DeleteGroup Text
dggOrganizationId = lens _dggOrganizationId (\ s a -> s{_dggOrganizationId = a})

-- | The identifier of the group to be deleted.
dggGroupId :: Lens' DeleteGroup Text
dggGroupId = lens _dggGroupId (\ s a -> s{_dggGroupId = a})

instance AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteGroup where

instance NFData DeleteGroup where

instance ToHeaders DeleteGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DeleteGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteGroup where
        toJSON DeleteGroup'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _dggOrganizationId),
                  Just ("GroupId" .= _dggGroupId)])

instance ToPath DeleteGroup where
        toPath = const "/"

instance ToQuery DeleteGroup where
        toQuery = const mempty

-- | /See:/ 'deleteGroupResponse' smart constructor.
newtype DeleteGroupResponse = DeleteGroupResponse'
  { _dgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrsResponseStatus' - -- | The response status code.
deleteGroupResponse
    :: Int -- ^ 'dgrsResponseStatus'
    -> DeleteGroupResponse
deleteGroupResponse pResponseStatus_ =
  DeleteGroupResponse' {_dgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dgrsResponseStatus :: Lens' DeleteGroupResponse Int
dgrsResponseStatus = lens _dgrsResponseStatus (\ s a -> s{_dgrsResponseStatus = a})

instance NFData DeleteGroupResponse where
