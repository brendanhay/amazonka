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
-- Module      : Network.AWS.XRay.UpdateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group resource.
--
--
module Network.AWS.XRay.UpdateGroup
    (
    -- * Creating a Request
      updateGroup
    , UpdateGroup
    -- * Request Lenses
    , ugFilterExpression
    , ugGroupARN
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
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'updateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { _ugFilterExpression :: !(Maybe Text)
  , _ugGroupARN         :: !(Maybe Text)
  , _ugGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugFilterExpression' - The updated filter expression defining criteria by which to group traces.
--
-- * 'ugGroupARN' - The ARN that was generated upon creation.
--
-- * 'ugGroupName' - The case-sensitive name of the group.
updateGroup
    :: UpdateGroup
updateGroup =
  UpdateGroup'
    { _ugFilterExpression = Nothing
    , _ugGroupARN = Nothing
    , _ugGroupName = Nothing
    }


-- | The updated filter expression defining criteria by which to group traces.
ugFilterExpression :: Lens' UpdateGroup (Maybe Text)
ugFilterExpression = lens _ugFilterExpression (\ s a -> s{_ugFilterExpression = a})

-- | The ARN that was generated upon creation.
ugGroupARN :: Lens' UpdateGroup (Maybe Text)
ugGroupARN = lens _ugGroupARN (\ s a -> s{_ugGroupARN = a})

-- | The case-sensitive name of the group.
ugGroupName :: Lens' UpdateGroup (Maybe Text)
ugGroupName = lens _ugGroupName (\ s a -> s{_ugGroupName = a})

instance AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        request = postJSON xRay
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
              (catMaybes
                 [("FilterExpression" .=) <$> _ugFilterExpression,
                  ("GroupARN" .=) <$> _ugGroupARN,
                  ("GroupName" .=) <$> _ugGroupName])

instance ToPath UpdateGroup where
        toPath = const "/UpdateGroup"

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
-- * 'ugrsGroup' - The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, and the updated filter expression assigned to the group.
--
-- * 'ugrsResponseStatus' - -- | The response status code.
updateGroupResponse
    :: Int -- ^ 'ugrsResponseStatus'
    -> UpdateGroupResponse
updateGroupResponse pResponseStatus_ =
  UpdateGroupResponse'
    {_ugrsGroup = Nothing, _ugrsResponseStatus = pResponseStatus_}


-- | The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, and the updated filter expression assigned to the group.
ugrsGroup :: Lens' UpdateGroupResponse (Maybe Group)
ugrsGroup = lens _ugrsGroup (\ s a -> s{_ugrsGroup = a})

-- | -- | The response status code.
ugrsResponseStatus :: Lens' UpdateGroupResponse Int
ugrsResponseStatus = lens _ugrsResponseStatus (\ s a -> s{_ugrsResponseStatus = a})

instance NFData UpdateGroupResponse where
