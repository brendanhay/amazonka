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
-- Module      : Network.AWS.ResourceGroups.UpdateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing group with a new or changed description. You cannot update the name of a resource group.
--
--
module Network.AWS.ResourceGroups.UpdateGroup
    (
    -- * Creating a Request
      updateGroup
    , UpdateGroup
    -- * Request Lenses
    , ugDescription
    , ugGroupName

    -- * Destructuring the Response
    , updateGroupResponse
    , UpdateGroupResponse
    -- * Response Lenses
    , ugrsGroup
    , ugrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { _ugDescription :: !(Maybe Text)
  , _ugGroupName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugDescription' - The description of the resource group. Descriptions can have a maximum of 511 characters, including letters, numbers, hyphens, underscores, punctuation, and spaces.
--
-- * 'ugGroupName' - The name of the resource group for which you want to update its description.
updateGroup
    :: Text -- ^ 'ugGroupName'
    -> UpdateGroup
updateGroup pGroupName_ =
  UpdateGroup' {_ugDescription = Nothing, _ugGroupName = pGroupName_}


-- | The description of the resource group. Descriptions can have a maximum of 511 characters, including letters, numbers, hyphens, underscores, punctuation, and spaces.
ugDescription :: Lens' UpdateGroup (Maybe Text)
ugDescription = lens _ugDescription (\ s a -> s{_ugDescription = a})

-- | The name of the resource group for which you want to update its description.
ugGroupName :: Lens' UpdateGroup Text
ugGroupName = lens _ugGroupName (\ s a -> s{_ugGroupName = a})

instance AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        request = putJSON resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable UpdateGroup where

instance NFData UpdateGroup where

instance ToHeaders UpdateGroup where
        toHeaders = const mempty

instance ToJSON UpdateGroup where
        toJSON UpdateGroup'{..}
          = object
              (catMaybes [("Description" .=) <$> _ugDescription])

instance ToPath UpdateGroup where
        toPath UpdateGroup'{..}
          = mconcat ["/groups/", toBS _ugGroupName]

instance ToQuery UpdateGroup where
        toQuery = const mempty

-- | /See:/ 'updateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { _ugrsGroup          :: !(Maybe Group)
  , _ugrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugrsGroup' - The full description of the resource group after it has been updated.
--
-- * 'ugrsResponseStatus' - -- | The response status code.
updateGroupResponse
    :: Int -- ^ 'ugrsResponseStatus'
    -> UpdateGroupResponse
updateGroupResponse pResponseStatus_ =
  UpdateGroupResponse'
    {_ugrsGroup = Nothing, _ugrsResponseStatus = pResponseStatus_}


-- | The full description of the resource group after it has been updated.
ugrsGroup :: Lens' UpdateGroupResponse (Maybe Group)
ugrsGroup = lens _ugrsGroup (\ s a -> s{_ugrsGroup = a})

-- | -- | The response status code.
ugrsResponseStatus :: Lens' UpdateGroupResponse Int
ugrsResponseStatus = lens _ugrsResponseStatus (\ s a -> s{_ugrsResponseStatus = a})

instance NFData UpdateGroupResponse where
