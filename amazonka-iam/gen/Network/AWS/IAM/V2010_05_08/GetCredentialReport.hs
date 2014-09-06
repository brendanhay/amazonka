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
    -- ** Response lenses
    , gcrrsrsContent
    , gcrrsrsReportFormat
    , gcrrsrsGeneratedTime
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
{-# INLINE mkGetCredentialReport #-}

instance ToQuery GetCredentialReport where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetCredentialReport
-- action.
data GetCredentialReportResponse = GetCredentialReportResponse
    { _gcrrsrsContent :: Maybe ByteString
    , _gcrrsrsReportFormat :: Maybe ReportFormatType
    , _gcrrsrsGeneratedTime :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Contains the credential report. The report is Base64-encoded.
gcrrsrsContent :: Lens' GetCredentialReportResponse (Maybe ByteString)
gcrrsrsContent = lens _gcrrsrsContent (\s a -> s { _gcrrsrsContent = a })
{-# INLINE gcrrsrsContent #-}

-- | The format (MIME type) of the credential report.
gcrrsrsReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
gcrrsrsReportFormat =
    lens _gcrrsrsReportFormat (\s a -> s { _gcrrsrsReportFormat = a })
{-# INLINE gcrrsrsReportFormat #-}

-- | The time and date when the credential report was created, in ISO 8601
-- date-time format.
gcrrsrsGeneratedTime :: Lens' GetCredentialReportResponse (Maybe ISO8601)
gcrrsrsGeneratedTime =
    lens _gcrrsrsGeneratedTime (\s a -> s { _gcrrsrsGeneratedTime = a })
{-# INLINE gcrrsrsGeneratedTime #-}

instance FromXML GetCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetCredentialReport where
    type Sv GetCredentialReport = IAM
    type Rs GetCredentialReport = GetCredentialReportResponse

    request = post "GetCredentialReport"
    response _ = xmlResponse
