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
-- Module      : Network.AWS.Greengrass.DeleteGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group.
module Network.AWS.Greengrass.DeleteGroup
    (
    -- * Creating a Request
      deleteGroup
    , DeleteGroup
    -- * Request Lenses
    , dgGroupId

    -- * Destructuring the Response
    , deleteGroupResponse
    , DeleteGroupResponse
    -- * Response Lenses
    , dgrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup'
  { _dgGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgGroupId' - The ID of the AWS Greengrass group.
deleteGroup
    :: Text -- ^ 'dgGroupId'
    -> DeleteGroup
deleteGroup pGroupId_ = DeleteGroup' {_dgGroupId = pGroupId_}


-- | The ID of the AWS Greengrass group.
dgGroupId :: Lens' DeleteGroup Text
dgGroupId = lens _dgGroupId (\ s a -> s{_dgGroupId = a})

instance AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        request = delete greengrass
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
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteGroup where
        toPath DeleteGroup'{..}
          = mconcat ["/greengrass/groups/", toBS _dgGroupId]

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
