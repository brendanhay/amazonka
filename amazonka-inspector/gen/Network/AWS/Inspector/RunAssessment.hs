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
-- Module      : Network.AWS.Inspector.RunAssessment
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the analysis of the application’s behavior against selected rule
-- packages for the assessment specified by the assessment ARN.
module Network.AWS.Inspector.RunAssessment
    (
    -- * Creating a Request
      runAssessment
    , RunAssessment
    -- * Request Lenses
    , raAssessmentARN
    , raRunName

    -- * Destructuring the Response
    , runAssessmentResponse
    , RunAssessmentResponse
    -- * Response Lenses
    , rarsRunARN
    , rarsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runAssessment' smart constructor.
data RunAssessment = RunAssessment'
    { _raAssessmentARN :: !Text
    , _raRunName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunAssessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAssessmentARN'
--
-- * 'raRunName'
runAssessment
    :: Text -- ^ 'raAssessmentARN'
    -> Text -- ^ 'raRunName'
    -> RunAssessment
runAssessment pAssessmentARN_ pRunName_ =
    RunAssessment'
    { _raAssessmentARN = pAssessmentARN_
    , _raRunName = pRunName_
    }

-- | The ARN of the assessment that you want to run.
raAssessmentARN :: Lens' RunAssessment Text
raAssessmentARN = lens _raAssessmentARN (\ s a -> s{_raAssessmentARN = a});

-- | A name specifying the run of the assessment.
raRunName :: Lens' RunAssessment Text
raRunName = lens _raRunName (\ s a -> s{_raRunName = a});

instance AWSRequest RunAssessment where
        type Rs RunAssessment = RunAssessmentResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 RunAssessmentResponse' <$>
                   (x .?> "runArn") <*> (pure (fromEnum s)))

instance Hashable RunAssessment

instance NFData RunAssessment

instance ToHeaders RunAssessment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.RunAssessment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RunAssessment where
        toJSON RunAssessment'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _raAssessmentARN),
                  Just ("runName" .= _raRunName)])

instance ToPath RunAssessment where
        toPath = const "/"

instance ToQuery RunAssessment where
        toQuery = const mempty

-- | /See:/ 'runAssessmentResponse' smart constructor.
data RunAssessmentResponse = RunAssessmentResponse'
    { _rarsRunARN         :: !(Maybe Text)
    , _rarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunAssessmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarsRunARN'
--
-- * 'rarsResponseStatus'
runAssessmentResponse
    :: Int -- ^ 'rarsResponseStatus'
    -> RunAssessmentResponse
runAssessmentResponse pResponseStatus_ =
    RunAssessmentResponse'
    { _rarsRunARN = Nothing
    , _rarsResponseStatus = pResponseStatus_
    }

-- | The ARN specifying the run of the assessment.
rarsRunARN :: Lens' RunAssessmentResponse (Maybe Text)
rarsRunARN = lens _rarsRunARN (\ s a -> s{_rarsRunARN = a});

-- | The response status code.
rarsResponseStatus :: Lens' RunAssessmentResponse Int
rarsResponseStatus = lens _rarsResponseStatus (\ s a -> s{_rarsResponseStatus = a});
