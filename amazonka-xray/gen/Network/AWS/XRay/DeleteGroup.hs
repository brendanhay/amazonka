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
-- Module      : Network.AWS.XRay.DeleteGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group resource.
--
--
module Network.AWS.XRay.DeleteGroup
    (
    -- * Creating a Request
      deleteGroup
    , DeleteGroup
    -- * Request Lenses
    , dgGroupARN
    , dgGroupName

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
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'deleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { _dgGroupARN :: !(Maybe Text)
  , _dgGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgGroupARN' - The ARN of the group that was generated on creation.
--
-- * 'dgGroupName' - The case-sensitive name of the group.
deleteGroup
    :: DeleteGroup
deleteGroup = DeleteGroup' {_dgGroupARN = Nothing, _dgGroupName = Nothing}


-- | The ARN of the group that was generated on creation.
dgGroupARN :: Lens' DeleteGroup (Maybe Text)
dgGroupARN = lens _dgGroupARN (\ s a -> s{_dgGroupARN = a})

-- | The case-sensitive name of the group.
dgGroupName :: Lens' DeleteGroup (Maybe Text)
dgGroupName = lens _dgGroupName (\ s a -> s{_dgGroupName = a})

instance AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        request = postJSON xRay
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteGroup where

instance NFData DeleteGroup where

instance ToHeaders DeleteGroup where
        toHeaders = const mempty

instance ToJSON DeleteGroup where
        toJSON DeleteGroup'{..}
          = object
              (catMaybes
                 [("GroupARN" .=) <$> _dgGroupARN,
                  ("GroupName" .=) <$> _dgGroupName])

instance ToPath DeleteGroup where
        toPath = const "/DeleteGroup"

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
