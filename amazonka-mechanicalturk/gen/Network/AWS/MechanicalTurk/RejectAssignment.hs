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
-- Module      : Network.AWS.MechanicalTurk.RejectAssignment
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectAssignment@ operation rejects the results of a completed assignment.
--
--
-- You can include an optional feedback message with the rejection, which the Worker can see in the Status section of the web site. When you include a feedback message with the rejection, it helps the Worker understand why the assignment was rejected, and can improve the quality of the results the Worker submits in the future.
--
-- Only the Requester who created the HIT can reject an assignment for the HIT.
--
module Network.AWS.MechanicalTurk.RejectAssignment
    (
    -- * Creating a Request
      rejectAssignment
    , RejectAssignment
    -- * Request Lenses
    , raRequesterFeedback
    , raAssignmentId

    -- * Destructuring the Response
    , rejectAssignmentResponse
    , RejectAssignmentResponse
    -- * Response Lenses
    , rarsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.MechanicalTurk.Types
import           Network.AWS.MechanicalTurk.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'rejectAssignment' smart constructor.
data RejectAssignment = RejectAssignment'
    { _raRequesterFeedback :: !(Maybe Text)
    , _raAssignmentId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RejectAssignment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raRequesterFeedback' - A message for the Worker, which the Worker can see in the Status section of the web site.
--
-- * 'raAssignmentId' - The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
rejectAssignment
    :: Text -- ^ 'raAssignmentId'
    -> RejectAssignment
rejectAssignment pAssignmentId_ =
    RejectAssignment'
    { _raRequesterFeedback = Nothing
    , _raAssignmentId = pAssignmentId_
    }

-- | A message for the Worker, which the Worker can see in the Status section of the web site.
raRequesterFeedback :: Lens' RejectAssignment (Maybe Text)
raRequesterFeedback = lens _raRequesterFeedback (\ s a -> s{_raRequesterFeedback = a});

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
raAssignmentId :: Lens' RejectAssignment Text
raAssignmentId = lens _raAssignmentId (\ s a -> s{_raAssignmentId = a});

instance AWSRequest RejectAssignment where
        type Rs RejectAssignment = RejectAssignmentResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 RejectAssignmentResponse' <$> (pure (fromEnum s)))

instance Hashable RejectAssignment

instance NFData RejectAssignment

instance ToHeaders RejectAssignment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.RejectAssignment" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RejectAssignment where
        toJSON RejectAssignment'{..}
          = object
              (catMaybes
                 [("RequesterFeedback" .=) <$> _raRequesterFeedback,
                  Just ("AssignmentId" .= _raAssignmentId)])

instance ToPath RejectAssignment where
        toPath = const "/"

instance ToQuery RejectAssignment where
        toQuery = const mempty

-- | /See:/ 'rejectAssignmentResponse' smart constructor.
newtype RejectAssignmentResponse = RejectAssignmentResponse'
    { _rarsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RejectAssignmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarsResponseStatus' - -- | The response status code.
rejectAssignmentResponse
    :: Int -- ^ 'rarsResponseStatus'
    -> RejectAssignmentResponse
rejectAssignmentResponse pResponseStatus_ =
    RejectAssignmentResponse'
    { _rarsResponseStatus = pResponseStatus_
    }

-- | -- | The response status code.
rarsResponseStatus :: Lens' RejectAssignmentResponse Int
rarsResponseStatus = lens _rarsResponseStatus (\ s a -> s{_rarsResponseStatus = a});

instance NFData RejectAssignmentResponse
