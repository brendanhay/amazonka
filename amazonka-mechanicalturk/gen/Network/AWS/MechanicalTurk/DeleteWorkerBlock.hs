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
-- Module      : Network.AWS.MechanicalTurk.DeleteWorkerBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteWorkerBlock@ operation allows you to reinstate a blocked Worker to work on your HITs. This operation reverses the effects of the CreateWorkerBlock operation. You need the Worker ID to use this operation. If the Worker ID is missing or invalid, this operation fails and returns the message “WorkerId is invalid.” If the specified Worker is not blocked, this operation returns successfully.
--
--
module Network.AWS.MechanicalTurk.DeleteWorkerBlock
    (
    -- * Creating a Request
      deleteWorkerBlock
    , DeleteWorkerBlock
    -- * Request Lenses
    , dwbReason
    , dwbWorkerId

    -- * Destructuring the Response
    , deleteWorkerBlockResponse
    , DeleteWorkerBlockResponse
    -- * Response Lenses
    , dwbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteWorkerBlock' smart constructor.
data DeleteWorkerBlock = DeleteWorkerBlock'
  { _dwbReason   :: !(Maybe Text)
  , _dwbWorkerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWorkerBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwbReason' - A message that explains the reason for unblocking the Worker. The Worker does not see this message.
--
-- * 'dwbWorkerId' - The ID of the Worker to unblock.
deleteWorkerBlock
    :: Text -- ^ 'dwbWorkerId'
    -> DeleteWorkerBlock
deleteWorkerBlock pWorkerId_ =
  DeleteWorkerBlock' {_dwbReason = Nothing, _dwbWorkerId = pWorkerId_}


-- | A message that explains the reason for unblocking the Worker. The Worker does not see this message.
dwbReason :: Lens' DeleteWorkerBlock (Maybe Text)
dwbReason = lens _dwbReason (\ s a -> s{_dwbReason = a})

-- | The ID of the Worker to unblock.
dwbWorkerId :: Lens' DeleteWorkerBlock Text
dwbWorkerId = lens _dwbWorkerId (\ s a -> s{_dwbWorkerId = a})

instance AWSRequest DeleteWorkerBlock where
        type Rs DeleteWorkerBlock = DeleteWorkerBlockResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteWorkerBlockResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteWorkerBlock where

instance NFData DeleteWorkerBlock where

instance ToHeaders DeleteWorkerBlock where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.DeleteWorkerBlock"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteWorkerBlock where
        toJSON DeleteWorkerBlock'{..}
          = object
              (catMaybes
                 [("Reason" .=) <$> _dwbReason,
                  Just ("WorkerId" .= _dwbWorkerId)])

instance ToPath DeleteWorkerBlock where
        toPath = const "/"

instance ToQuery DeleteWorkerBlock where
        toQuery = const mempty

-- | /See:/ 'deleteWorkerBlockResponse' smart constructor.
newtype DeleteWorkerBlockResponse = DeleteWorkerBlockResponse'
  { _dwbrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWorkerBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwbrsResponseStatus' - -- | The response status code.
deleteWorkerBlockResponse
    :: Int -- ^ 'dwbrsResponseStatus'
    -> DeleteWorkerBlockResponse
deleteWorkerBlockResponse pResponseStatus_ =
  DeleteWorkerBlockResponse' {_dwbrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dwbrsResponseStatus :: Lens' DeleteWorkerBlockResponse Int
dwbrsResponseStatus = lens _dwbrsResponseStatus (\ s a -> s{_dwbrsResponseStatus = a})

instance NFData DeleteWorkerBlockResponse where
