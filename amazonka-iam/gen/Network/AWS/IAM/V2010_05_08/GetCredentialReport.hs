{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetCredentialReport
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves a credential report for the AWS account. For more information
-- about the credential report, see Getting Credential Reports in the Using
-- IAM guide.
module Network.AWS.IAM.V2010_05_08.GetCredentialReport
    (
    -- * Request
      GetCredentialReport
    -- ** Request constructor
    , mkGetCredentialReport
    -- * Response
    , GetCredentialReportResponse
    -- ** Response constructor
    , mkGetCredentialReportResponse
    -- ** Response lenses
    , gcrrrContent
    , gcrrrReportFormat
    , gcrrrGeneratedTime
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data GetCredentialReport = GetCredentialReport
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCredentialReport' request.
mkGetCredentialReport :: GetCredentialReport
mkGetCredentialReport = GetCredentialReport

instance ToQuery GetCredentialReport where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetCredentialReport
-- action.
data GetCredentialReportResponse = GetCredentialReportResponse
    { _gcrrrContent :: Maybe ByteString
    , _gcrrrReportFormat :: Maybe ReportFormatType
    , _gcrrrGeneratedTime :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCredentialReportResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkGetCredentialReportResponse :: GetCredentialReportResponse
mkGetCredentialReportResponse = GetCredentialReportResponse
    { _gcrrrContent = Nothing
    , _gcrrrReportFormat = Nothing
    , _gcrrrGeneratedTime = Nothing
    }

-- | Contains the credential report. The report is Base64-encoded.
gcrrrContent :: Lens' GetCredentialReportResponse (Maybe ByteString)
gcrrrContent = lens _gcrrrContent (\s a -> s { _gcrrrContent = a })

-- | The format (MIME type) of the credential report.
gcrrrReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
gcrrrReportFormat =
    lens _gcrrrReportFormat (\s a -> s { _gcrrrReportFormat = a })

-- | The time and date when the credential report was created, in ISO 8601
-- date-time format.
gcrrrGeneratedTime :: Lens' GetCredentialReportResponse (Maybe ISO8601)
gcrrrGeneratedTime =
    lens _gcrrrGeneratedTime (\s a -> s { _gcrrrGeneratedTime = a })

instance FromXML GetCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetCredentialReport where
    type Sv GetCredentialReport = IAM
    type Rs GetCredentialReport = GetCredentialReportResponse

    request = post "GetCredentialReport"
    response _ = xmlResponse
