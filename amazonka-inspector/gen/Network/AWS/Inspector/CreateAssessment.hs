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
-- Module      : Network.AWS.Inspector.CreateAssessment
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an assessment for the application specified by the application
-- ARN. You can create up to 500 assessments per AWS account.
module Network.AWS.Inspector.CreateAssessment
    (
    -- * Creating a Request
      createAssessment
    , CreateAssessment
    -- * Request Lenses
    , caUserAttributesForFindings
    , caApplicationARN
    , caAssessmentName
    , caDurationInSeconds

    -- * Destructuring the Response
    , createAssessmentResponse
    , CreateAssessmentResponse
    -- * Response Lenses
    , crsAssessmentARN
    , crsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAssessment' smart constructor.
data CreateAssessment = CreateAssessment'
    { _caUserAttributesForFindings :: !(Maybe [Attribute])
    , _caApplicationARN            :: !Text
    , _caAssessmentName            :: !Text
    , _caDurationInSeconds         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caUserAttributesForFindings'
--
-- * 'caApplicationARN'
--
-- * 'caAssessmentName'
--
-- * 'caDurationInSeconds'
createAssessment
    :: Text -- ^ 'caApplicationARN'
    -> Text -- ^ 'caAssessmentName'
    -> Int -- ^ 'caDurationInSeconds'
    -> CreateAssessment
createAssessment pApplicationARN_ pAssessmentName_ pDurationInSeconds_ =
    CreateAssessment'
    { _caUserAttributesForFindings = Nothing
    , _caApplicationARN = pApplicationARN_
    , _caAssessmentName = pAssessmentName_
    , _caDurationInSeconds = pDurationInSeconds_
    }

-- | The user-defined attributes that are assigned to every finding generated
-- by running this assessment.
caUserAttributesForFindings :: Lens' CreateAssessment [Attribute]
caUserAttributesForFindings = lens _caUserAttributesForFindings (\ s a -> s{_caUserAttributesForFindings = a}) . _Default . _Coerce;

-- | The ARN specifying the application for which you want to create an
-- assessment.
caApplicationARN :: Lens' CreateAssessment Text
caApplicationARN = lens _caApplicationARN (\ s a -> s{_caApplicationARN = a});

-- | The user-defined name identifying the assessment that you want to
-- create. You can create several assessments for an application. The names
-- of the assessments corresponding to a particular application must be
-- unique.
caAssessmentName :: Lens' CreateAssessment Text
caAssessmentName = lens _caAssessmentName (\ s a -> s{_caAssessmentName = a});

-- | The duration of the assessment in seconds. The default value is 3600
-- seconds (one hour). The maximum value is 86400 seconds (one day).
caDurationInSeconds :: Lens' CreateAssessment Int
caDurationInSeconds = lens _caDurationInSeconds (\ s a -> s{_caDurationInSeconds = a});

instance AWSRequest CreateAssessment where
        type Rs CreateAssessment = CreateAssessmentResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssessmentResponse' <$>
                   (x .?> "assessmentArn") <*> (pure (fromEnum s)))

instance Hashable CreateAssessment

instance ToHeaders CreateAssessment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.CreateAssessment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAssessment where
        toJSON CreateAssessment'{..}
          = object
              (catMaybes
                 [("userAttributesForFindings" .=) <$>
                    _caUserAttributesForFindings,
                  Just ("applicationArn" .= _caApplicationARN),
                  Just ("assessmentName" .= _caAssessmentName),
                  Just ("durationInSeconds" .= _caDurationInSeconds)])

instance ToPath CreateAssessment where
        toPath = const "/"

instance ToQuery CreateAssessment where
        toQuery = const mempty

-- | /See:/ 'createAssessmentResponse' smart constructor.
data CreateAssessmentResponse = CreateAssessmentResponse'
    { _crsAssessmentARN  :: !(Maybe Text)
    , _crsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssessmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsAssessmentARN'
--
-- * 'crsResponseStatus'
createAssessmentResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateAssessmentResponse
createAssessmentResponse pResponseStatus_ =
    CreateAssessmentResponse'
    { _crsAssessmentARN = Nothing
    , _crsResponseStatus = pResponseStatus_
    }

-- | The ARN specifying the assessment that is created.
crsAssessmentARN :: Lens' CreateAssessmentResponse (Maybe Text)
crsAssessmentARN = lens _crsAssessmentARN (\ s a -> s{_crsAssessmentARN = a});

-- | The response status code.
crsResponseStatus :: Lens' CreateAssessmentResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a});
