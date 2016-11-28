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
-- Module      : Network.AWS.Inspector.StopAssessmentRun
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the assessment run that is specified by the ARN of the assessment run.
--
--
module Network.AWS.Inspector.StopAssessmentRun
    (
    -- * Creating a Request
      stopAssessmentRun
    , StopAssessmentRun
    -- * Request Lenses
    , sarAssessmentRunARN

    -- * Destructuring the Response
    , stopAssessmentRunResponse
    , StopAssessmentRunResponse
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopAssessmentRun' smart constructor.
newtype StopAssessmentRun = StopAssessmentRun'
    { _sarAssessmentRunARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopAssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarAssessmentRunARN' - The ARN of the assessment run that you want to stop.
stopAssessmentRun
    :: Text -- ^ 'sarAssessmentRunARN'
    -> StopAssessmentRun
stopAssessmentRun pAssessmentRunARN_ =
    StopAssessmentRun'
    { _sarAssessmentRunARN = pAssessmentRunARN_
    }

-- | The ARN of the assessment run that you want to stop.
sarAssessmentRunARN :: Lens' StopAssessmentRun Text
sarAssessmentRunARN = lens _sarAssessmentRunARN (\ s a -> s{_sarAssessmentRunARN = a});

instance AWSRequest StopAssessmentRun where
        type Rs StopAssessmentRun = StopAssessmentRunResponse
        request = postJSON inspector
        response = receiveNull StopAssessmentRunResponse'

instance Hashable StopAssessmentRun

instance NFData StopAssessmentRun

instance ToHeaders StopAssessmentRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.StopAssessmentRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopAssessmentRun where
        toJSON StopAssessmentRun'{..}
          = object
              (catMaybes
                 [Just ("assessmentRunArn" .= _sarAssessmentRunARN)])

instance ToPath StopAssessmentRun where
        toPath = const "/"

instance ToQuery StopAssessmentRun where
        toQuery = const mempty

-- | /See:/ 'stopAssessmentRunResponse' smart constructor.
data StopAssessmentRunResponse =
    StopAssessmentRunResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopAssessmentRunResponse' with the minimum fields required to make a request.
--
stopAssessmentRunResponse
    :: StopAssessmentRunResponse
stopAssessmentRunResponse = StopAssessmentRunResponse'

instance NFData StopAssessmentRunResponse
