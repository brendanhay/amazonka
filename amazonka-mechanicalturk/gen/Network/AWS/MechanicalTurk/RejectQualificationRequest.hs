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
-- Module      : Network.AWS.MechanicalTurk.RejectQualificationRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectQualificationRequest@ operation rejects a user's request for a Qualification.
--
--
-- You can provide a text message explaining why the request was rejected. The Worker who made the request can see this message.
--
module Network.AWS.MechanicalTurk.RejectQualificationRequest
    (
    -- * Creating a Request
      rejectQualificationRequest
    , RejectQualificationRequest
    -- * Request Lenses
    , rqrReason
    , rqrQualificationRequestId

    -- * Destructuring the Response
    , rejectQualificationRequestResponse
    , RejectQualificationRequestResponse
    -- * Response Lenses
    , rqrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rejectQualificationRequest' smart constructor.
data RejectQualificationRequest = RejectQualificationRequest'
  { _rqrReason                 :: !(Maybe Text)
  , _rqrQualificationRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectQualificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqrReason' - A text message explaining why the request was rejected, to be shown to the Worker who made the request.
--
-- * 'rqrQualificationRequestId' - The ID of the Qualification request, as returned by the @ListQualificationRequests@ operation.
rejectQualificationRequest
    :: Text -- ^ 'rqrQualificationRequestId'
    -> RejectQualificationRequest
rejectQualificationRequest pQualificationRequestId_ =
  RejectQualificationRequest'
    { _rqrReason = Nothing
    , _rqrQualificationRequestId = pQualificationRequestId_
    }


-- | A text message explaining why the request was rejected, to be shown to the Worker who made the request.
rqrReason :: Lens' RejectQualificationRequest (Maybe Text)
rqrReason = lens _rqrReason (\ s a -> s{_rqrReason = a})

-- | The ID of the Qualification request, as returned by the @ListQualificationRequests@ operation.
rqrQualificationRequestId :: Lens' RejectQualificationRequest Text
rqrQualificationRequestId = lens _rqrQualificationRequestId (\ s a -> s{_rqrQualificationRequestId = a})

instance AWSRequest RejectQualificationRequest where
        type Rs RejectQualificationRequest =
             RejectQualificationRequestResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 RejectQualificationRequestResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RejectQualificationRequest where

instance NFData RejectQualificationRequest where

instance ToHeaders RejectQualificationRequest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.RejectQualificationRequest"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RejectQualificationRequest where
        toJSON RejectQualificationRequest'{..}
          = object
              (catMaybes
                 [("Reason" .=) <$> _rqrReason,
                  Just
                    ("QualificationRequestId" .=
                       _rqrQualificationRequestId)])

instance ToPath RejectQualificationRequest where
        toPath = const "/"

instance ToQuery RejectQualificationRequest where
        toQuery = const mempty

-- | /See:/ 'rejectQualificationRequestResponse' smart constructor.
newtype RejectQualificationRequestResponse = RejectQualificationRequestResponse'
  { _rqrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectQualificationRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqrrsResponseStatus' - -- | The response status code.
rejectQualificationRequestResponse
    :: Int -- ^ 'rqrrsResponseStatus'
    -> RejectQualificationRequestResponse
rejectQualificationRequestResponse pResponseStatus_ =
  RejectQualificationRequestResponse' {_rqrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rqrrsResponseStatus :: Lens' RejectQualificationRequestResponse Int
rqrrsResponseStatus = lens _rqrrsResponseStatus (\ s a -> s{_rqrrsResponseStatus = a})

instance NFData RejectQualificationRequestResponse
         where
