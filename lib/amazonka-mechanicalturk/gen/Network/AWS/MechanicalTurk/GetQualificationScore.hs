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
-- Module      : Network.AWS.MechanicalTurk.GetQualificationScore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationScore@ operation returns the value of a Worker's Qualification for a given Qualification type.
--
--
-- To get a Worker's Qualification, you must know the Worker's ID. The Worker's ID is included in the assignment data returned by the @ListAssignmentsForHIT@ operation.
--
-- Only the owner of a Qualification type can query the value of a Worker's Qualification of that type.
--
module Network.AWS.MechanicalTurk.GetQualificationScore
    (
    -- * Creating a Request
      getQualificationScore
    , GetQualificationScore
    -- * Request Lenses
    , gqsQualificationTypeId
    , gqsWorkerId

    -- * Destructuring the Response
    , getQualificationScoreResponse
    , GetQualificationScoreResponse
    -- * Response Lenses
    , gqsrsQualification
    , gqsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQualificationScore' smart constructor.
data GetQualificationScore = GetQualificationScore'
  { _gqsQualificationTypeId :: !Text
  , _gqsWorkerId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQualificationScore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqsQualificationTypeId' - The ID of the QualificationType.
--
-- * 'gqsWorkerId' - The ID of the Worker whose Qualification is being updated.
getQualificationScore
    :: Text -- ^ 'gqsQualificationTypeId'
    -> Text -- ^ 'gqsWorkerId'
    -> GetQualificationScore
getQualificationScore pQualificationTypeId_ pWorkerId_ =
  GetQualificationScore'
    {_gqsQualificationTypeId = pQualificationTypeId_, _gqsWorkerId = pWorkerId_}


-- | The ID of the QualificationType.
gqsQualificationTypeId :: Lens' GetQualificationScore Text
gqsQualificationTypeId = lens _gqsQualificationTypeId (\ s a -> s{_gqsQualificationTypeId = a})

-- | The ID of the Worker whose Qualification is being updated.
gqsWorkerId :: Lens' GetQualificationScore Text
gqsWorkerId = lens _gqsWorkerId (\ s a -> s{_gqsWorkerId = a})

instance AWSRequest GetQualificationScore where
        type Rs GetQualificationScore =
             GetQualificationScoreResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 GetQualificationScoreResponse' <$>
                   (x .?> "Qualification") <*> (pure (fromEnum s)))

instance Hashable GetQualificationScore where

instance NFData GetQualificationScore where

instance ToHeaders GetQualificationScore where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.GetQualificationScore"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetQualificationScore where
        toJSON GetQualificationScore'{..}
          = object
              (catMaybes
                 [Just
                    ("QualificationTypeId" .= _gqsQualificationTypeId),
                  Just ("WorkerId" .= _gqsWorkerId)])

instance ToPath GetQualificationScore where
        toPath = const "/"

instance ToQuery GetQualificationScore where
        toQuery = const mempty

-- | /See:/ 'getQualificationScoreResponse' smart constructor.
data GetQualificationScoreResponse = GetQualificationScoreResponse'
  { _gqsrsQualification  :: !(Maybe Qualification)
  , _gqsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQualificationScoreResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqsrsQualification' - The Qualification data structure of the Qualification assigned to a user, including the Qualification type and the value (score).
--
-- * 'gqsrsResponseStatus' - -- | The response status code.
getQualificationScoreResponse
    :: Int -- ^ 'gqsrsResponseStatus'
    -> GetQualificationScoreResponse
getQualificationScoreResponse pResponseStatus_ =
  GetQualificationScoreResponse'
    {_gqsrsQualification = Nothing, _gqsrsResponseStatus = pResponseStatus_}


-- | The Qualification data structure of the Qualification assigned to a user, including the Qualification type and the value (score).
gqsrsQualification :: Lens' GetQualificationScoreResponse (Maybe Qualification)
gqsrsQualification = lens _gqsrsQualification (\ s a -> s{_gqsrsQualification = a})

-- | -- | The response status code.
gqsrsResponseStatus :: Lens' GetQualificationScoreResponse Int
gqsrsResponseStatus = lens _gqsrsResponseStatus (\ s a -> s{_gqsrsResponseStatus = a})

instance NFData GetQualificationScoreResponse where
