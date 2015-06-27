{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.GetCredentialReport
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves a credential report for the AWS account. For more information
-- about the credential report, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetCredentialReport.html>
module Network.AWS.IAM.GetCredentialReport
    (
    -- * Request
      GetCredentialReport
    -- ** Request constructor
    , getCredentialReport

    -- * Response
    , GetCredentialReportResponse
    -- ** Response constructor
    , getCredentialReportResponse
    -- ** Response lenses
    , getContent
    , getGeneratedTime
    , getReportFormat
    , getStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getCredentialReport' smart constructor.
data GetCredentialReport =
    GetCredentialReport'
    deriving (Eq,Read,Show)

-- | 'GetCredentialReport' smart constructor.
getCredentialReport :: GetCredentialReport
getCredentialReport = GetCredentialReport'

instance AWSRequest GetCredentialReport where
        type Sv GetCredentialReport = IAM
        type Rs GetCredentialReport =
             GetCredentialReportResponse
        request = post
        response
          = receiveXMLWrapper "GetCredentialReportResult"
              (\ s h x ->
                 GetCredentialReportResponse' <$>
                   (x .@? "Content") <*> (x .@? "GeneratedTime") <*>
                     (x .@? "ReportFormat")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetCredentialReport where
        toHeaders = const mempty

instance ToPath GetCredentialReport where
        toPath = const "/"

instance ToQuery GetCredentialReport where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("GetCredentialReport" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful GetCredentialReport request.
--
-- /See:/ 'getCredentialReportResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'getContent'
--
-- * 'getGeneratedTime'
--
-- * 'getReportFormat'
--
-- * 'getStatus'
data GetCredentialReportResponse = GetCredentialReportResponse'
    { _getContent       :: Maybe Base64
    , _getGeneratedTime :: Maybe ISO8601
    , _getReportFormat  :: Maybe ReportFormatType
    , _getStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetCredentialReportResponse' smart constructor.
getCredentialReportResponse :: Int -> GetCredentialReportResponse
getCredentialReportResponse pStatus =
    GetCredentialReportResponse'
    { _getContent = Nothing
    , _getGeneratedTime = Nothing
    , _getReportFormat = Nothing
    , _getStatus = pStatus
    }

-- | Contains the credential report. The report is Base64-encoded.
getContent :: Lens' GetCredentialReportResponse (Maybe Base64)
getContent = lens _getContent (\ s a -> s{_getContent = a});

-- | The date and time when the credential report was created, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>.
getGeneratedTime :: Lens' GetCredentialReportResponse (Maybe UTCTime)
getGeneratedTime = lens _getGeneratedTime (\ s a -> s{_getGeneratedTime = a}) . mapping _Time;

-- | The format (MIME type) of the credential report.
getReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
getReportFormat = lens _getReportFormat (\ s a -> s{_getReportFormat = a});

-- | FIXME: Undocumented member.
getStatus :: Lens' GetCredentialReportResponse Int
getStatus = lens _getStatus (\ s a -> s{_getStatus = a});
