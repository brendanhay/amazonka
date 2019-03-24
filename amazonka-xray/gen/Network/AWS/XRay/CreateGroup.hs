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
-- Module      : Network.AWS.XRay.CreateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group resource with a name and a filter expression. 
--
--
module Network.AWS.XRay.CreateGroup
    (
    -- * Creating a Request
      createGroup
    , CreateGroup
    -- * Request Lenses
    , cgFilterExpression
    , cgGroupName

    -- * Destructuring the Response
    , createGroupResponse
    , CreateGroupResponse
    -- * Response Lenses
    , cgrsGroup
    , cgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgFilterExpression :: !(Maybe Text)
  , _cgGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgFilterExpression' - The filter expression defining criteria by which to group traces.
--
-- * 'cgGroupName' - The case-sensitive name of the new group. Default is a reserved name and names must be unique.
createGroup
    :: Text -- ^ 'cgGroupName'
    -> CreateGroup
createGroup pGroupName_ =
  CreateGroup' {_cgFilterExpression = Nothing, _cgGroupName = pGroupName_}


-- | The filter expression defining criteria by which to group traces.
cgFilterExpression :: Lens' CreateGroup (Maybe Text)
cgFilterExpression = lens _cgFilterExpression (\ s a -> s{_cgFilterExpression = a})

-- | The case-sensitive name of the new group. Default is a reserved name and names must be unique.
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\ s a -> s{_cgGroupName = a})

instance AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 CreateGroupResponse' <$>
                   (x .?> "Group") <*> (pure (fromEnum s)))

instance Hashable CreateGroup where

instance NFData CreateGroup where

instance ToHeaders CreateGroup where
        toHeaders = const mempty

instance ToJSON CreateGroup where
        toJSON CreateGroup'{..}
          = object
              (catMaybes
                 [("FilterExpression" .=) <$> _cgFilterExpression,
                  Just ("GroupName" .= _cgGroupName)])

instance ToPath CreateGroup where
        toPath = const "/CreateGroup"

instance ToQuery CreateGroup where
        toQuery = const mempty

-- | /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsGroup :: !(Maybe Group)
  , _cgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGroup' - The group that was created. Contains the name of the group that was created, the ARN of the group that was generated based on the group name, and the filter expression that was assigned to the group.
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGroupResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> CreateGroupResponse
createGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    {_cgrsGroup = Nothing, _cgrsResponseStatus = pResponseStatus_}


-- | The group that was created. Contains the name of the group that was created, the ARN of the group that was generated based on the group name, and the filter expression that was assigned to the group.
cgrsGroup :: Lens' CreateGroupResponse (Maybe Group)
cgrsGroup = lens _cgrsGroup (\ s a -> s{_cgrsGroup = a})

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a})

instance NFData CreateGroupResponse where
