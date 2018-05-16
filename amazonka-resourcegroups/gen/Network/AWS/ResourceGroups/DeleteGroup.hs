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
-- Module      : Network.AWS.ResourceGroups.DeleteGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified resource group. Deleting a resource group does not delete resources that are members of the group; it only deletes the group structure.
--
--
module Network.AWS.ResourceGroups.DeleteGroup
    (
    -- * Creating a Request
      deleteGroup
    , DeleteGroup
    -- * Request Lenses
    , dgGroupName

    -- * Destructuring the Response
    , deleteGroupResponse
    , DeleteGroupResponse
    -- * Response Lenses
    , dgrsGroup
    , dgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'deleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup'
  { _dgGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgGroupName' - The name of the resource group to delete.
deleteGroup
    :: Text -- ^ 'dgGroupName'
    -> DeleteGroup
deleteGroup pGroupName_ = DeleteGroup' {_dgGroupName = pGroupName_}


-- | The name of the resource group to delete.
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\ s a -> s{_dgGroupName = a})

instance AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        request = delete resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 DeleteGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable DeleteGroup where

instance NFData DeleteGroup where

instance ToHeaders DeleteGroup where
        toHeaders = const mempty

instance ToPath DeleteGroup where
        toPath DeleteGroup'{..}
          = mconcat ["/groups/", toBS _dgGroupName]

instance ToQuery DeleteGroup where
        toQuery = const mempty

-- | /See:/ 'deleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { _dgrsGroup          :: !(Maybe Group)
  , _dgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrsGroup' - A full description of the deleted resource group.
--
-- * 'dgrsResponseStatus' - -- | The response status code.
deleteGroupResponse
    :: Int -- ^ 'dgrsResponseStatus'
    -> DeleteGroupResponse
deleteGroupResponse pResponseStatus_ =
  DeleteGroupResponse'
    {_dgrsGroup = Nothing, _dgrsResponseStatus = pResponseStatus_}


-- | A full description of the deleted resource group.
dgrsGroup :: Lens' DeleteGroupResponse (Maybe Group)
dgrsGroup = lens _dgrsGroup (\ s a -> s{_dgrsGroup = a})

-- | -- | The response status code.
dgrsResponseStatus :: Lens' DeleteGroupResponse Int
dgrsResponseStatus = lens _dgrsResponseStatus (\ s a -> s{_dgrsResponseStatus = a})

instance NFData DeleteGroupResponse where
