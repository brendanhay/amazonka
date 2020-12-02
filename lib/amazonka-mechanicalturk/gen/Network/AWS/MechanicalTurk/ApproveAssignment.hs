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
-- Module      : Network.AWS.MechanicalTurk.ApproveAssignment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ApproveAssignment@ operation approves the results of a completed assignment.
--
--
-- Approving an assignment initiates two payments from the Requester's Amazon.com account
--
--     * The Worker who submitted the results is paid the reward specified in the HIT.
--
--     * Amazon Mechanical Turk fees are debited.
--
--
--
-- If the Requester's account does not have adequate funds for these payments, the call to ApproveAssignment returns an exception, and the approval is not processed. You can include an optional feedback message with the approval, which the Worker can see in the Status section of the web site.
--
-- You can also call this operation for assignments that were previous rejected and approve them by explicitly overriding the previous rejection. This only works on rejected assignments that were submitted within the previous 30 days and only if the assignment's related HIT has not been deleted.
--
module Network.AWS.MechanicalTurk.ApproveAssignment
    (
    -- * Creating a Request
      approveAssignment
    , ApproveAssignment
    -- * Request Lenses
    , aaOverrideRejection
    , aaRequesterFeedback
    , aaAssignmentId

    -- * Destructuring the Response
    , approveAssignmentResponse
    , ApproveAssignmentResponse
    -- * Response Lenses
    , aarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'approveAssignment' smart constructor.
data ApproveAssignment = ApproveAssignment'
  { _aaOverrideRejection :: !(Maybe Bool)
  , _aaRequesterFeedback :: !(Maybe Text)
  , _aaAssignmentId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApproveAssignment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaOverrideRejection' - A flag indicating that an assignment should be approved even if it was previously rejected. Defaults to @False@ .
--
-- * 'aaRequesterFeedback' - A message for the Worker, which the Worker can see in the Status section of the web site.
--
-- * 'aaAssignmentId' - The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
approveAssignment
    :: Text -- ^ 'aaAssignmentId'
    -> ApproveAssignment
approveAssignment pAssignmentId_ =
  ApproveAssignment'
    { _aaOverrideRejection = Nothing
    , _aaRequesterFeedback = Nothing
    , _aaAssignmentId = pAssignmentId_
    }


-- | A flag indicating that an assignment should be approved even if it was previously rejected. Defaults to @False@ .
aaOverrideRejection :: Lens' ApproveAssignment (Maybe Bool)
aaOverrideRejection = lens _aaOverrideRejection (\ s a -> s{_aaOverrideRejection = a})

-- | A message for the Worker, which the Worker can see in the Status section of the web site.
aaRequesterFeedback :: Lens' ApproveAssignment (Maybe Text)
aaRequesterFeedback = lens _aaRequesterFeedback (\ s a -> s{_aaRequesterFeedback = a})

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
aaAssignmentId :: Lens' ApproveAssignment Text
aaAssignmentId = lens _aaAssignmentId (\ s a -> s{_aaAssignmentId = a})

instance AWSRequest ApproveAssignment where
        type Rs ApproveAssignment = ApproveAssignmentResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 ApproveAssignmentResponse' <$> (pure (fromEnum s)))

instance Hashable ApproveAssignment where

instance NFData ApproveAssignment where

instance ToHeaders ApproveAssignment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ApproveAssignment"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ApproveAssignment where
        toJSON ApproveAssignment'{..}
          = object
              (catMaybes
                 [("OverrideRejection" .=) <$> _aaOverrideRejection,
                  ("RequesterFeedback" .=) <$> _aaRequesterFeedback,
                  Just ("AssignmentId" .= _aaAssignmentId)])

instance ToPath ApproveAssignment where
        toPath = const "/"

instance ToQuery ApproveAssignment where
        toQuery = const mempty

-- | /See:/ 'approveAssignmentResponse' smart constructor.
newtype ApproveAssignmentResponse = ApproveAssignmentResponse'
  { _aarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApproveAssignmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aarsResponseStatus' - -- | The response status code.
approveAssignmentResponse
    :: Int -- ^ 'aarsResponseStatus'
    -> ApproveAssignmentResponse
approveAssignmentResponse pResponseStatus_ =
  ApproveAssignmentResponse' {_aarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aarsResponseStatus :: Lens' ApproveAssignmentResponse Int
aarsResponseStatus = lens _aarsResponseStatus (\ s a -> s{_aarsResponseStatus = a})

instance NFData ApproveAssignmentResponse where
