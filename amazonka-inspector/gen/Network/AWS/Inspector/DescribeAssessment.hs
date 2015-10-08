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
-- Module      : Network.AWS.Inspector.DescribeAssessment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment specified by the assessment ARN.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_DescribeAssessment.html AWS API Reference> for DescribeAssessment.
module Network.AWS.Inspector.DescribeAssessment
    (
    -- * Creating a Request
      describeAssessment
    , DescribeAssessment
    -- * Request Lenses
    , dAssessmentARN

    -- * Destructuring the Response
    , describeAssessmentResponse
    , DescribeAssessmentResponse
    -- * Response Lenses
    , desrsAssessment
    , desrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAssessment' smart constructor.
newtype DescribeAssessment = DescribeAssessment'
    { _dAssessmentARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAssessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAssessmentARN'
describeAssessment
    :: DescribeAssessment
describeAssessment =
    DescribeAssessment'
    { _dAssessmentARN = Nothing
    }

-- | The ARN specifying the assessment that you want to describe.
dAssessmentARN :: Lens' DescribeAssessment (Maybe Text)
dAssessmentARN = lens _dAssessmentARN (\ s a -> s{_dAssessmentARN = a});

instance AWSRequest DescribeAssessment where
        type Rs DescribeAssessment =
             DescribeAssessmentResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssessmentResponse' <$>
                   (x .?> "assessment") <*> (pure (fromEnum s)))

instance ToHeaders DescribeAssessment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeAssessment" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssessment where
        toJSON DescribeAssessment'{..}
          = object
              (catMaybes
                 [("assessmentArn" .=) <$> _dAssessmentARN])

instance ToPath DescribeAssessment where
        toPath = const "/"

instance ToQuery DescribeAssessment where
        toQuery = const mempty

-- | /See:/ 'describeAssessmentResponse' smart constructor.
data DescribeAssessmentResponse = DescribeAssessmentResponse'
    { _desrsAssessment     :: !(Maybe Assessment)
    , _desrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAssessmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsAssessment'
--
-- * 'desrsResponseStatus'
describeAssessmentResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeAssessmentResponse
describeAssessmentResponse pResponseStatus_ =
    DescribeAssessmentResponse'
    { _desrsAssessment = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }

-- | Information about the assessment.
desrsAssessment :: Lens' DescribeAssessmentResponse (Maybe Assessment)
desrsAssessment = lens _desrsAssessment (\ s a -> s{_desrsAssessment = a});

-- | The response status code.
desrsResponseStatus :: Lens' DescribeAssessmentResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a});
