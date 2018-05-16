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
-- Module      : Network.AWS.WorkSpaces.DeleteIPGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP access control group.
--
--
-- You cannot delete an IP access control group that is associated with a directory.
--
module Network.AWS.WorkSpaces.DeleteIPGroup
    (
    -- * Creating a Request
      deleteIPGroup
    , DeleteIPGroup
    -- * Request Lenses
    , digGroupId

    -- * Destructuring the Response
    , deleteIPGroupResponse
    , DeleteIPGroupResponse
    -- * Response Lenses
    , dipgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'deleteIPGroup' smart constructor.
newtype DeleteIPGroup = DeleteIPGroup'
  { _digGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIPGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digGroupId' - The ID of the IP access control group.
deleteIPGroup
    :: Text -- ^ 'digGroupId'
    -> DeleteIPGroup
deleteIPGroup pGroupId_ = DeleteIPGroup' {_digGroupId = pGroupId_}


-- | The ID of the IP access control group.
digGroupId :: Lens' DeleteIPGroup Text
digGroupId = lens _digGroupId (\ s a -> s{_digGroupId = a})

instance AWSRequest DeleteIPGroup where
        type Rs DeleteIPGroup = DeleteIPGroupResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteIPGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteIPGroup where

instance NFData DeleteIPGroup where

instance ToHeaders DeleteIPGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DeleteIpGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteIPGroup where
        toJSON DeleteIPGroup'{..}
          = object
              (catMaybes [Just ("GroupId" .= _digGroupId)])

instance ToPath DeleteIPGroup where
        toPath = const "/"

instance ToQuery DeleteIPGroup where
        toQuery = const mempty

-- | /See:/ 'deleteIPGroupResponse' smart constructor.
newtype DeleteIPGroupResponse = DeleteIPGroupResponse'
  { _dipgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIPGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipgrsResponseStatus' - -- | The response status code.
deleteIPGroupResponse
    :: Int -- ^ 'dipgrsResponseStatus'
    -> DeleteIPGroupResponse
deleteIPGroupResponse pResponseStatus_ =
  DeleteIPGroupResponse' {_dipgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dipgrsResponseStatus :: Lens' DeleteIPGroupResponse Int
dipgrsResponseStatus = lens _dipgrsResponseStatus (\ s a -> s{_dipgrsResponseStatus = a})

instance NFData DeleteIPGroupResponse where
