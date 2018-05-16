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
-- Module      : Network.AWS.GameLift.DeleteGameSessionQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a game session queue. This action means that any 'StartGameSessionPlacement' requests that reference this queue will fail. To delete a queue, specify the queue name.
--
--
-- Queue-related operations include:
--
--     * 'CreateGameSessionQueue'
--
--     * 'DescribeGameSessionQueues'
--
--     * 'UpdateGameSessionQueue'
--
--     * 'DeleteGameSessionQueue'
--
--
--
module Network.AWS.GameLift.DeleteGameSessionQueue
    (
    -- * Creating a Request
      deleteGameSessionQueue
    , DeleteGameSessionQueue
    -- * Request Lenses
    , dgsqName

    -- * Destructuring the Response
    , deleteGameSessionQueueResponse
    , DeleteGameSessionQueueResponse
    -- * Response Lenses
    , dgsqrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'deleteGameSessionQueue' smart constructor.
newtype DeleteGameSessionQueue = DeleteGameSessionQueue'
  { _dgsqName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGameSessionQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsqName' - Descriptive label that is associated with game session queue. Queue names must be unique within each region.
deleteGameSessionQueue
    :: Text -- ^ 'dgsqName'
    -> DeleteGameSessionQueue
deleteGameSessionQueue pName_ = DeleteGameSessionQueue' {_dgsqName = pName_}


-- | Descriptive label that is associated with game session queue. Queue names must be unique within each region.
dgsqName :: Lens' DeleteGameSessionQueue Text
dgsqName = lens _dgsqName (\ s a -> s{_dgsqName = a})

instance AWSRequest DeleteGameSessionQueue where
        type Rs DeleteGameSessionQueue =
             DeleteGameSessionQueueResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteGameSessionQueueResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteGameSessionQueue where

instance NFData DeleteGameSessionQueue where

instance ToHeaders DeleteGameSessionQueue where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DeleteGameSessionQueue" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteGameSessionQueue where
        toJSON DeleteGameSessionQueue'{..}
          = object (catMaybes [Just ("Name" .= _dgsqName)])

instance ToPath DeleteGameSessionQueue where
        toPath = const "/"

instance ToQuery DeleteGameSessionQueue where
        toQuery = const mempty

-- | /See:/ 'deleteGameSessionQueueResponse' smart constructor.
newtype DeleteGameSessionQueueResponse = DeleteGameSessionQueueResponse'
  { _dgsqrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGameSessionQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsqrsResponseStatus' - -- | The response status code.
deleteGameSessionQueueResponse
    :: Int -- ^ 'dgsqrsResponseStatus'
    -> DeleteGameSessionQueueResponse
deleteGameSessionQueueResponse pResponseStatus_ =
  DeleteGameSessionQueueResponse' {_dgsqrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dgsqrsResponseStatus :: Lens' DeleteGameSessionQueueResponse Int
dgsqrsResponseStatus = lens _dgsqrsResponseStatus (\ s a -> s{_dgsqrsResponseStatus = a})

instance NFData DeleteGameSessionQueueResponse where
