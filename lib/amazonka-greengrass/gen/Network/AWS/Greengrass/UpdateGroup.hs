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
-- Module      : Network.AWS.Greengrass.UpdateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group.
module Network.AWS.Greengrass.UpdateGroup
    (
    -- * Creating a Request
      updateGroup
    , UpdateGroup
    -- * Request Lenses
    , ugName
    , ugGroupId

    -- * Destructuring the Response
    , updateGroupResponse
    , UpdateGroupResponse
    -- * Response Lenses
    , ugrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { _ugName    :: !(Maybe Text)
  , _ugGroupId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugName' - The name of the definition.
--
-- * 'ugGroupId' - The ID of the AWS Greengrass group.
updateGroup
    :: Text -- ^ 'ugGroupId'
    -> UpdateGroup
updateGroup pGroupId_ = UpdateGroup' {_ugName = Nothing, _ugGroupId = pGroupId_}


-- | The name of the definition.
ugName :: Lens' UpdateGroup (Maybe Text)
ugName = lens _ugName (\ s a -> s{_ugName = a})

-- | The ID of the AWS Greengrass group.
ugGroupId :: Lens' UpdateGroup Text
ugGroupId = lens _ugGroupId (\ s a -> s{_ugGroupId = a})

instance AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateGroupResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateGroup where

instance NFData UpdateGroup where

instance ToHeaders UpdateGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGroup where
        toJSON UpdateGroup'{..}
          = object (catMaybes [("Name" .=) <$> _ugName])

instance ToPath UpdateGroup where
        toPath UpdateGroup'{..}
          = mconcat ["/greengrass/groups/", toBS _ugGroupId]

instance ToQuery UpdateGroup where
        toQuery = const mempty

-- | /See:/ 'updateGroupResponse' smart constructor.
newtype UpdateGroupResponse = UpdateGroupResponse'
  { _ugrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugrsResponseStatus' - -- | The response status code.
updateGroupResponse
    :: Int -- ^ 'ugrsResponseStatus'
    -> UpdateGroupResponse
updateGroupResponse pResponseStatus_ =
  UpdateGroupResponse' {_ugrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ugrsResponseStatus :: Lens' UpdateGroupResponse Int
ugrsResponseStatus = lens _ugrsResponseStatus (\ s a -> s{_ugrsResponseStatus = a})

instance NFData UpdateGroupResponse where
