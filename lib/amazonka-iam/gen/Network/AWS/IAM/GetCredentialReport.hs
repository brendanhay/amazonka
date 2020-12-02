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
-- Module      : Network.AWS.IAM.GetCredentialReport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a credential report for the AWS account. For more information about the credential report, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.GetCredentialReport
    (
    -- * Creating a Request
      getCredentialReport
    , GetCredentialReport

    -- * Destructuring the Response
    , getCredentialReportResponse
    , GetCredentialReportResponse
    -- * Response Lenses
    , grsContent
    , grsGeneratedTime
    , grsReportFormat
    , grsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCredentialReport' smart constructor.
data GetCredentialReport =
  GetCredentialReport'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCredentialReport' with the minimum fields required to make a request.
--
getCredentialReport
    :: GetCredentialReport
getCredentialReport = GetCredentialReport'


instance AWSRequest GetCredentialReport where
        type Rs GetCredentialReport =
             GetCredentialReportResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetCredentialReportResult"
              (\ s h x ->
                 GetCredentialReportResponse' <$>
                   (x .@? "Content") <*> (x .@? "GeneratedTime") <*>
                     (x .@? "ReportFormat")
                     <*> (pure (fromEnum s)))

instance Hashable GetCredentialReport where

instance NFData GetCredentialReport where

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

-- | Contains the response to a successful 'GetCredentialReport' request.
--
--
--
-- /See:/ 'getCredentialReportResponse' smart constructor.
data GetCredentialReportResponse = GetCredentialReportResponse'
  { _grsContent        :: !(Maybe Base64)
  , _grsGeneratedTime  :: !(Maybe ISO8601)
  , _grsReportFormat   :: !(Maybe ReportFormatType)
  , _grsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCredentialReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsContent' - Contains the credential report. The report is Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'grsGeneratedTime' - The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> .
--
-- * 'grsReportFormat' - The format (MIME type) of the credential report.
--
-- * 'grsResponseStatus' - -- | The response status code.
getCredentialReportResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetCredentialReportResponse
getCredentialReportResponse pResponseStatus_ =
  GetCredentialReportResponse'
    { _grsContent = Nothing
    , _grsGeneratedTime = Nothing
    , _grsReportFormat = Nothing
    , _grsResponseStatus = pResponseStatus_
    }


-- | Contains the credential report. The report is Base64-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
grsContent :: Lens' GetCredentialReportResponse (Maybe ByteString)
grsContent = lens _grsContent (\ s a -> s{_grsContent = a}) . mapping _Base64

-- | The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> .
grsGeneratedTime :: Lens' GetCredentialReportResponse (Maybe UTCTime)
grsGeneratedTime = lens _grsGeneratedTime (\ s a -> s{_grsGeneratedTime = a}) . mapping _Time

-- | The format (MIME type) of the credential report.
grsReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
grsReportFormat = lens _grsReportFormat (\ s a -> s{_grsReportFormat = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetCredentialReportResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetCredentialReportResponse where
