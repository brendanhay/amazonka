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
-- Module      : Network.AWS.Inspector.GetTelemetryMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the data that is collected for the specified assessment run.
--
--
module Network.AWS.Inspector.GetTelemetryMetadata
    (
    -- * Creating a Request
      getTelemetryMetadata
    , GetTelemetryMetadata
    -- * Request Lenses
    , gtmAssessmentRunARN

    -- * Destructuring the Response
    , getTelemetryMetadataResponse
    , GetTelemetryMetadataResponse
    -- * Response Lenses
    , gtmrsResponseStatus
    , gtmrsTelemetryMetadata
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTelemetryMetadata' smart constructor.
newtype GetTelemetryMetadata = GetTelemetryMetadata'
  { _gtmAssessmentRunARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTelemetryMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtmAssessmentRunARN' - The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
getTelemetryMetadata
    :: Text -- ^ 'gtmAssessmentRunARN'
    -> GetTelemetryMetadata
getTelemetryMetadata pAssessmentRunARN_ =
  GetTelemetryMetadata' {_gtmAssessmentRunARN = pAssessmentRunARN_}


-- | The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
gtmAssessmentRunARN :: Lens' GetTelemetryMetadata Text
gtmAssessmentRunARN = lens _gtmAssessmentRunARN (\ s a -> s{_gtmAssessmentRunARN = a})

instance AWSRequest GetTelemetryMetadata where
        type Rs GetTelemetryMetadata =
             GetTelemetryMetadataResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 GetTelemetryMetadataResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "telemetryMetadata" .!@ mempty))

instance Hashable GetTelemetryMetadata where

instance NFData GetTelemetryMetadata where

instance ToHeaders GetTelemetryMetadata where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.GetTelemetryMetadata" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTelemetryMetadata where
        toJSON GetTelemetryMetadata'{..}
          = object
              (catMaybes
                 [Just ("assessmentRunArn" .= _gtmAssessmentRunARN)])

instance ToPath GetTelemetryMetadata where
        toPath = const "/"

instance ToQuery GetTelemetryMetadata where
        toQuery = const mempty

-- | /See:/ 'getTelemetryMetadataResponse' smart constructor.
data GetTelemetryMetadataResponse = GetTelemetryMetadataResponse'
  { _gtmrsResponseStatus    :: !Int
  , _gtmrsTelemetryMetadata :: ![TelemetryMetadata]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTelemetryMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtmrsResponseStatus' - -- | The response status code.
--
-- * 'gtmrsTelemetryMetadata' - Telemetry details.
getTelemetryMetadataResponse
    :: Int -- ^ 'gtmrsResponseStatus'
    -> GetTelemetryMetadataResponse
getTelemetryMetadataResponse pResponseStatus_ =
  GetTelemetryMetadataResponse'
    {_gtmrsResponseStatus = pResponseStatus_, _gtmrsTelemetryMetadata = mempty}


-- | -- | The response status code.
gtmrsResponseStatus :: Lens' GetTelemetryMetadataResponse Int
gtmrsResponseStatus = lens _gtmrsResponseStatus (\ s a -> s{_gtmrsResponseStatus = a})

-- | Telemetry details.
gtmrsTelemetryMetadata :: Lens' GetTelemetryMetadataResponse [TelemetryMetadata]
gtmrsTelemetryMetadata = lens _gtmrsTelemetryMetadata (\ s a -> s{_gtmrsTelemetryMetadata = a}) . _Coerce

instance NFData GetTelemetryMetadataResponse where
