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
-- Module      : Network.AWS.Inspector.StartDataCollection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts data collection for the assessment specified by the assessment
-- ARN. For this API to function properly, you must not exceed the limit of
-- running up to 500 concurrent agents per AWS account.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_StartDataCollection.html AWS API Reference> for StartDataCollection.
module Network.AWS.Inspector.StartDataCollection
    (
    -- * Creating a Request
      startDataCollection
    , StartDataCollection
    -- * Request Lenses
    , sdcAssessmentARN

    -- * Destructuring the Response
    , startDataCollectionResponse
    , StartDataCollectionResponse
    -- * Response Lenses
    , srsMessage
    , srsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startDataCollection' smart constructor.
newtype StartDataCollection = StartDataCollection'
    { _sdcAssessmentARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartDataCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcAssessmentARN'
startDataCollection
    :: Text -- ^ 'sdcAssessmentARN'
    -> StartDataCollection
startDataCollection pAssessmentARN_ =
    StartDataCollection'
    { _sdcAssessmentARN = pAssessmentARN_
    }

-- | The ARN of the assessment for which you want to start the data
-- collection process.
sdcAssessmentARN :: Lens' StartDataCollection Text
sdcAssessmentARN = lens _sdcAssessmentARN (\ s a -> s{_sdcAssessmentARN = a});

instance AWSRequest StartDataCollection where
        type Rs StartDataCollection =
             StartDataCollectionResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 StartDataCollectionResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance ToHeaders StartDataCollection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.StartDataCollection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartDataCollection where
        toJSON StartDataCollection'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _sdcAssessmentARN)])

instance ToPath StartDataCollection where
        toPath = const "/"

instance ToQuery StartDataCollection where
        toQuery = const mempty

-- | /See:/ 'startDataCollectionResponse' smart constructor.
data StartDataCollectionResponse = StartDataCollectionResponse'
    { _srsMessage        :: !(Maybe Text)
    , _srsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartDataCollectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsMessage'
--
-- * 'srsResponseStatus'
startDataCollectionResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartDataCollectionResponse
startDataCollectionResponse pResponseStatus_ =
    StartDataCollectionResponse'
    { _srsMessage = Nothing
    , _srsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
srsMessage :: Lens' StartDataCollectionResponse (Maybe Text)
srsMessage = lens _srsMessage (\ s a -> s{_srsMessage = a});

-- | The response status code.
srsResponseStatus :: Lens' StartDataCollectionResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a});
