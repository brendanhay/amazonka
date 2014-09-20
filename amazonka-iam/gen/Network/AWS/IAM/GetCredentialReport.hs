{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

-- | Retrieves a credential report for the AWS account. For more information
-- about the credential report, see Getting Credential Reports in the Using
-- IAM guide. https://iam.amazonaws.com/ ?Action=GetCredentialReport
-- &Version=2010-05-08 &AUTHPARAMS BASE-64 ENCODED FILE CONTENTS text/csv
-- 2014-08-28T21:42:50Z 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
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
    , gcrrrContent
    , gcrrrReportFormat
    , gcrrrGeneratedTime
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetCredentialReport = GetCredentialReport
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCredentialReport' request.
getCredentialReport :: GetCredentialReport
getCredentialReport = GetCredentialReport

instance ToQuery GetCredentialReport where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetCredentialReport
-- action.
data GetCredentialReportResponse = GetCredentialReportResponse
    { _gcrrrContent :: Maybe ByteString
    , _gcrrrReportFormat :: Maybe ReportFormatType
    , _gcrrrGeneratedTime :: Maybe ISO8601
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCredentialReportResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Content ::@ @Maybe ByteString@
--
-- * @ReportFormat ::@ @Maybe ReportFormatType@
--
-- * @GeneratedTime ::@ @Maybe ISO8601@
--
getCredentialReportResponse :: GetCredentialReportResponse
getCredentialReportResponse = GetCredentialReportResponse
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
