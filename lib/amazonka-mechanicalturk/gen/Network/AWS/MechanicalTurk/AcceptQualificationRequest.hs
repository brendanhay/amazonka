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
-- Module      : Network.AWS.MechanicalTurk.AcceptQualificationRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AcceptQualificationRequest@ operation approves a Worker's request for a Qualification.
--
--
-- Only the owner of the Qualification type can grant a Qualification request for that type.
--
-- A successful request for the @AcceptQualificationRequest@ operation returns with no errors and an empty body.
--
module Network.AWS.MechanicalTurk.AcceptQualificationRequest
    (
    -- * Creating a Request
      acceptQualificationRequest
    , AcceptQualificationRequest
    -- * Request Lenses
    , aqrIntegerValue
    , aqrQualificationRequestId

    -- * Destructuring the Response
    , acceptQualificationRequestResponse
    , AcceptQualificationRequestResponse
    -- * Response Lenses
    , aqrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptQualificationRequest' smart constructor.
data AcceptQualificationRequest = AcceptQualificationRequest'
  { _aqrIntegerValue           :: !(Maybe Int)
  , _aqrQualificationRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptQualificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqrIntegerValue' - The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement.
--
-- * 'aqrQualificationRequestId' - The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
acceptQualificationRequest
    :: Text -- ^ 'aqrQualificationRequestId'
    -> AcceptQualificationRequest
acceptQualificationRequest pQualificationRequestId_ =
  AcceptQualificationRequest'
    { _aqrIntegerValue = Nothing
    , _aqrQualificationRequestId = pQualificationRequestId_
    }


-- | The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement.
aqrIntegerValue :: Lens' AcceptQualificationRequest (Maybe Int)
aqrIntegerValue = lens _aqrIntegerValue (\ s a -> s{_aqrIntegerValue = a})

-- | The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
aqrQualificationRequestId :: Lens' AcceptQualificationRequest Text
aqrQualificationRequestId = lens _aqrQualificationRequestId (\ s a -> s{_aqrQualificationRequestId = a})

instance AWSRequest AcceptQualificationRequest where
        type Rs AcceptQualificationRequest =
             AcceptQualificationRequestResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 AcceptQualificationRequestResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AcceptQualificationRequest where

instance NFData AcceptQualificationRequest where

instance ToHeaders AcceptQualificationRequest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.AcceptQualificationRequest"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcceptQualificationRequest where
        toJSON AcceptQualificationRequest'{..}
          = object
              (catMaybes
                 [("IntegerValue" .=) <$> _aqrIntegerValue,
                  Just
                    ("QualificationRequestId" .=
                       _aqrQualificationRequestId)])

instance ToPath AcceptQualificationRequest where
        toPath = const "/"

instance ToQuery AcceptQualificationRequest where
        toQuery = const mempty

-- | /See:/ 'acceptQualificationRequestResponse' smart constructor.
newtype AcceptQualificationRequestResponse = AcceptQualificationRequestResponse'
  { _aqrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptQualificationRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqrrsResponseStatus' - -- | The response status code.
acceptQualificationRequestResponse
    :: Int -- ^ 'aqrrsResponseStatus'
    -> AcceptQualificationRequestResponse
acceptQualificationRequestResponse pResponseStatus_ =
  AcceptQualificationRequestResponse' {_aqrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aqrrsResponseStatus :: Lens' AcceptQualificationRequestResponse Int
aqrrsResponseStatus = lens _aqrrsResponseStatus (\ s a -> s{_aqrrsResponseStatus = a})

instance NFData AcceptQualificationRequestResponse
         where
