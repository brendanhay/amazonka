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
-- Module      : Network.AWS.Inspector.StopDataCollection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop data collection for the assessment specified by the assessment ARN.
module Network.AWS.Inspector.StopDataCollection
    (
    -- * Creating a Request
      stopDataCollection
    , StopDataCollection
    -- * Request Lenses
    , sAssessmentARN

    -- * Destructuring the Response
    , stopDataCollectionResponse
    , StopDataCollectionResponse
    -- * Response Lenses
    , sdcrsMessage
    , sdcrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopDataCollection' smart constructor.
newtype StopDataCollection = StopDataCollection'
    { _sAssessmentARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopDataCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAssessmentARN'
stopDataCollection
    :: Text -- ^ 'sAssessmentARN'
    -> StopDataCollection
stopDataCollection pAssessmentARN_ =
    StopDataCollection'
    { _sAssessmentARN = pAssessmentARN_
    }

-- | The ARN of the assessment for which you want to stop the data collection
-- process.
sAssessmentARN :: Lens' StopDataCollection Text
sAssessmentARN = lens _sAssessmentARN (\ s a -> s{_sAssessmentARN = a});

instance AWSRequest StopDataCollection where
        type Rs StopDataCollection =
             StopDataCollectionResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 StopDataCollectionResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance ToHeaders StopDataCollection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.StopDataCollection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopDataCollection where
        toJSON StopDataCollection'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _sAssessmentARN)])

instance ToPath StopDataCollection where
        toPath = const "/"

instance ToQuery StopDataCollection where
        toQuery = const mempty

-- | /See:/ 'stopDataCollectionResponse' smart constructor.
data StopDataCollectionResponse = StopDataCollectionResponse'
    { _sdcrsMessage        :: !(Maybe Text)
    , _sdcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopDataCollectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcrsMessage'
--
-- * 'sdcrsResponseStatus'
stopDataCollectionResponse
    :: Int -- ^ 'sdcrsResponseStatus'
    -> StopDataCollectionResponse
stopDataCollectionResponse pResponseStatus_ =
    StopDataCollectionResponse'
    { _sdcrsMessage = Nothing
    , _sdcrsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
sdcrsMessage :: Lens' StopDataCollectionResponse (Maybe Text)
sdcrsMessage = lens _sdcrsMessage (\ s a -> s{_sdcrsMessage = a});

-- | The response status code.
sdcrsResponseStatus :: Lens' StopDataCollectionResponse Int
sdcrsResponseStatus = lens _sdcrsResponseStatus (\ s a -> s{_sdcrsResponseStatus = a});
