{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetCredentialReport
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
-- about the credential report, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /Using IAM/
-- guide.
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
    , gcrrContent
    , gcrrGeneratedTime
    , gcrrReportFormat
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetCredentialReport = GetCredentialReport
    deriving (Eq, Ord, Show, Generic)

-- | 'GetCredentialReport' constructor.
getCredentialReport :: GetCredentialReport
getCredentialReport = GetCredentialReport

data GetCredentialReportResponse = GetCredentialReportResponse
    { _gcrrContent       :: Maybe Base64
    , _gcrrGeneratedTime :: Maybe ISO8601
    , _gcrrReportFormat  :: Maybe ReportFormatType
    } deriving (Eq, Show)

-- | 'GetCredentialReportResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrrContent' @::@ 'Maybe' 'Base64'
--
-- * 'gcrrGeneratedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gcrrReportFormat' @::@ 'Maybe' 'ReportFormatType'
--
getCredentialReportResponse :: GetCredentialReportResponse
getCredentialReportResponse = GetCredentialReportResponse
    { _gcrrContent       = Nothing
    , _gcrrReportFormat  = Nothing
    , _gcrrGeneratedTime = Nothing
    }

-- | Contains the credential report. The report is Base64-encoded.
gcrrContent :: Lens' GetCredentialReportResponse (Maybe Base64)
gcrrContent = lens _gcrrContent (\s a -> s { _gcrrContent = a })

-- | The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601date-time format>.
gcrrGeneratedTime :: Lens' GetCredentialReportResponse (Maybe UTCTime)
gcrrGeneratedTime =
    lens _gcrrGeneratedTime (\s a -> s { _gcrrGeneratedTime = a })
        . mapping _Time

-- | The format (MIME type) of the credential report.
gcrrReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
gcrrReportFormat = lens _gcrrReportFormat (\s a -> s { _gcrrReportFormat = a })

instance ToPath GetCredentialReport where
    toPath = const "/"

instance ToQuery GetCredentialReport where
    toQuery = const mempty

instance ToHeaders GetCredentialReport

instance AWSRequest GetCredentialReport where
    type Sv GetCredentialReport = IAM
    type Rs GetCredentialReport = GetCredentialReportResponse

    request  = post "GetCredentialReport"
    response = xmlResponse

instance FromXML GetCredentialReportResponse where
    parseXML = withElement "GetCredentialReportResult" $ \x -> GetCredentialReportResponse
        <$> x .@? "Content"
        <*> x .@? "GeneratedTime"
        <*> x .@? "ReportFormat"
