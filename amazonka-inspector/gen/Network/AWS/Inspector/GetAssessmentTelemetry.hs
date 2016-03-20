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
-- Module      : Network.AWS.Inspector.GetAssessmentTelemetry
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata about the telemetry (application behavioral data)
-- for the assessment specified by the assessment ARN.
module Network.AWS.Inspector.GetAssessmentTelemetry
    (
    -- * Creating a Request
      getAssessmentTelemetry
    , GetAssessmentTelemetry
    -- * Request Lenses
    , gatAssessmentARN

    -- * Destructuring the Response
    , getAssessmentTelemetryResponse
    , GetAssessmentTelemetryResponse
    -- * Response Lenses
    , gatrsTelemetry
    , gatrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getAssessmentTelemetry' smart constructor.
newtype GetAssessmentTelemetry = GetAssessmentTelemetry'
    { _gatAssessmentARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAssessmentTelemetry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatAssessmentARN'
getAssessmentTelemetry
    :: Text -- ^ 'gatAssessmentARN'
    -> GetAssessmentTelemetry
getAssessmentTelemetry pAssessmentARN_ =
    GetAssessmentTelemetry'
    { _gatAssessmentARN = pAssessmentARN_
    }

-- | The ARN specifying the assessment the telemetry of which you want to
-- obtain.
gatAssessmentARN :: Lens' GetAssessmentTelemetry Text
gatAssessmentARN = lens _gatAssessmentARN (\ s a -> s{_gatAssessmentARN = a});

instance AWSRequest GetAssessmentTelemetry where
        type Rs GetAssessmentTelemetry =
             GetAssessmentTelemetryResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 GetAssessmentTelemetryResponse' <$>
                   (x .?> "telemetry" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetAssessmentTelemetry

instance ToHeaders GetAssessmentTelemetry where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.GetAssessmentTelemetry" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAssessmentTelemetry where
        toJSON GetAssessmentTelemetry'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _gatAssessmentARN)])

instance ToPath GetAssessmentTelemetry where
        toPath = const "/"

instance ToQuery GetAssessmentTelemetry where
        toQuery = const mempty

-- | /See:/ 'getAssessmentTelemetryResponse' smart constructor.
data GetAssessmentTelemetryResponse = GetAssessmentTelemetryResponse'
    { _gatrsTelemetry      :: !(Maybe [Telemetry])
    , _gatrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAssessmentTelemetryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatrsTelemetry'
--
-- * 'gatrsResponseStatus'
getAssessmentTelemetryResponse
    :: Int -- ^ 'gatrsResponseStatus'
    -> GetAssessmentTelemetryResponse
getAssessmentTelemetryResponse pResponseStatus_ =
    GetAssessmentTelemetryResponse'
    { _gatrsTelemetry = Nothing
    , _gatrsResponseStatus = pResponseStatus_
    }

-- | Telemetry details.
gatrsTelemetry :: Lens' GetAssessmentTelemetryResponse [Telemetry]
gatrsTelemetry = lens _gatrsTelemetry (\ s a -> s{_gatrsTelemetry = a}) . _Default . _Coerce;

-- | The response status code.
gatrsResponseStatus :: Lens' GetAssessmentTelemetryResponse Int
gatrsResponseStatus = lens _gatrsResponseStatus (\ s a -> s{_gatrsResponseStatus = a});
