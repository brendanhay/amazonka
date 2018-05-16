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
-- Module      : Network.AWS.Inspector.GetAssessmentReport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces an assessment report that includes detailed and comprehensive results of a specified assessment run.
--
--
module Network.AWS.Inspector.GetAssessmentReport
    (
    -- * Creating a Request
      getAssessmentReport
    , GetAssessmentReport
    -- * Request Lenses
    , garAssessmentRunARN
    , garReportFileFormat
    , garReportType

    -- * Destructuring the Response
    , getAssessmentReportResponse
    , GetAssessmentReportResponse
    -- * Response Lenses
    , garrsUrl
    , garrsResponseStatus
    , garrsStatus
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAssessmentReport' smart constructor.
data GetAssessmentReport = GetAssessmentReport'
  { _garAssessmentRunARN :: !Text
  , _garReportFileFormat :: !ReportFileFormat
  , _garReportType       :: !ReportType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAssessmentReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garAssessmentRunARN' - The ARN that specifies the assessment run for which you want to generate a report.
--
-- * 'garReportFileFormat' - Specifies the file format (html or pdf) of the assessment report that you want to generate.
--
-- * 'garReportType' - Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <http://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
getAssessmentReport
    :: Text -- ^ 'garAssessmentRunARN'
    -> ReportFileFormat -- ^ 'garReportFileFormat'
    -> ReportType -- ^ 'garReportType'
    -> GetAssessmentReport
getAssessmentReport pAssessmentRunARN_ pReportFileFormat_ pReportType_ =
  GetAssessmentReport'
    { _garAssessmentRunARN = pAssessmentRunARN_
    , _garReportFileFormat = pReportFileFormat_
    , _garReportType = pReportType_
    }


-- | The ARN that specifies the assessment run for which you want to generate a report.
garAssessmentRunARN :: Lens' GetAssessmentReport Text
garAssessmentRunARN = lens _garAssessmentRunARN (\ s a -> s{_garAssessmentRunARN = a})

-- | Specifies the file format (html or pdf) of the assessment report that you want to generate.
garReportFileFormat :: Lens' GetAssessmentReport ReportFileFormat
garReportFileFormat = lens _garReportFileFormat (\ s a -> s{_garReportFileFormat = a})

-- | Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <http://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
garReportType :: Lens' GetAssessmentReport ReportType
garReportType = lens _garReportType (\ s a -> s{_garReportType = a})

instance AWSRequest GetAssessmentReport where
        type Rs GetAssessmentReport =
             GetAssessmentReportResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 GetAssessmentReportResponse' <$>
                   (x .?> "url") <*> (pure (fromEnum s)) <*>
                     (x .:> "status"))

instance Hashable GetAssessmentReport where

instance NFData GetAssessmentReport where

instance ToHeaders GetAssessmentReport where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.GetAssessmentReport" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAssessmentReport where
        toJSON GetAssessmentReport'{..}
          = object
              (catMaybes
                 [Just ("assessmentRunArn" .= _garAssessmentRunARN),
                  Just ("reportFileFormat" .= _garReportFileFormat),
                  Just ("reportType" .= _garReportType)])

instance ToPath GetAssessmentReport where
        toPath = const "/"

instance ToQuery GetAssessmentReport where
        toQuery = const mempty

-- | /See:/ 'getAssessmentReportResponse' smart constructor.
data GetAssessmentReportResponse = GetAssessmentReportResponse'
  { _garrsUrl            :: !(Maybe Text)
  , _garrsResponseStatus :: !Int
  , _garrsStatus         :: !ReportStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAssessmentReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garrsUrl' - Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
--
-- * 'garrsResponseStatus' - -- | The response status code.
--
-- * 'garrsStatus' - Specifies the status of the request to generate an assessment report.
getAssessmentReportResponse
    :: Int -- ^ 'garrsResponseStatus'
    -> ReportStatus -- ^ 'garrsStatus'
    -> GetAssessmentReportResponse
getAssessmentReportResponse pResponseStatus_ pStatus_ =
  GetAssessmentReportResponse'
    { _garrsUrl = Nothing
    , _garrsResponseStatus = pResponseStatus_
    , _garrsStatus = pStatus_
    }


-- | Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
garrsUrl :: Lens' GetAssessmentReportResponse (Maybe Text)
garrsUrl = lens _garrsUrl (\ s a -> s{_garrsUrl = a})

-- | -- | The response status code.
garrsResponseStatus :: Lens' GetAssessmentReportResponse Int
garrsResponseStatus = lens _garrsResponseStatus (\ s a -> s{_garrsResponseStatus = a})

-- | Specifies the status of the request to generate an assessment report.
garrsStatus :: Lens' GetAssessmentReportResponse ReportStatus
garrsStatus = lens _garrsStatus (\ s a -> s{_garrsStatus = a})

instance NFData GetAssessmentReportResponse where
