{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetCredentialReport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a credential report for the AWS account. For more information
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
    , grsContent
    , grsGeneratedTime
    , grsReportFormat
    , grsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getCredentialReport' smart constructor.
data GetCredentialReport =
    GetCredentialReport'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCredentialReport' smart constructor.
getCredentialReport :: GetCredentialReport
getCredentialReport = GetCredentialReport'

instance AWSRequest GetCredentialReport where
        type Sv GetCredentialReport = IAM
        type Rs GetCredentialReport =
             GetCredentialReportResponse
        request = postQuery
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
-- * 'grsContent'
--
-- * 'grsGeneratedTime'
--
-- * 'grsReportFormat'
--
-- * 'grsStatus'
data GetCredentialReportResponse = GetCredentialReportResponse'
    { _grsContent       :: !(Maybe Base64)
    , _grsGeneratedTime :: !(Maybe ISO8601)
    , _grsReportFormat  :: !(Maybe ReportFormatType)
    , _grsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCredentialReportResponse' smart constructor.
getCredentialReportResponse :: Int -> GetCredentialReportResponse
getCredentialReportResponse pStatus_ =
    GetCredentialReportResponse'
    { _grsContent = Nothing
    , _grsGeneratedTime = Nothing
    , _grsReportFormat = Nothing
    , _grsStatus = pStatus_
    }

-- | Contains the credential report. The report is Base64-encoded.
grsContent :: Lens' GetCredentialReportResponse (Maybe Base64)
grsContent = lens _grsContent (\ s a -> s{_grsContent = a});

-- | The date and time when the credential report was created, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>.
grsGeneratedTime :: Lens' GetCredentialReportResponse (Maybe UTCTime)
grsGeneratedTime = lens _grsGeneratedTime (\ s a -> s{_grsGeneratedTime = a}) . mapping _Time;

-- | The format (MIME type) of the credential report.
grsReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
grsReportFormat = lens _grsReportFormat (\ s a -> s{_grsReportFormat = a});

-- | FIXME: Undocumented member.
grsStatus :: Lens' GetCredentialReportResponse Int
grsStatus = lens _grsStatus (\ s a -> s{_grsStatus = a});
