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
    , getCredentialReport
    -- * Response
    , GetCredentialReportResponse
    -- ** Response lenses
    , gcrsGeneratedTime
    , gcrsContent
    , gcrsReportFormat
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetCredentialReport' request.
getCredentialReport :: GetCredentialReport
getCredentialReport = GetCredentialReport
{-# INLINE getCredentialReport #-}

data GetCredentialReport = GetCredentialReport
    deriving (Eq, Show, Generic)

instance ToQuery GetCredentialReport where
    toQuery = genericQuery def

data GetCredentialReportResponse = GetCredentialReportResponse
    { _gcrsGeneratedTime :: Maybe ISO8601
      -- ^ The time and date when the credential report was created, in ISO
      -- 8601 date-time format.
    , _gcrsContent :: Maybe ByteString
      -- ^ Contains the credential report. The report is Base64-encoded.
    , _gcrsReportFormat :: Maybe ReportFormatType
      -- ^ The format (MIME type) of the credential report.
    } deriving (Show, Generic)

-- | The time and date when the credential report was created, in ISO 8601
-- date-time format.
gcrsGeneratedTime :: Lens' GetCredentialReportResponse (Maybe ISO8601)
gcrsGeneratedTime f x =
    f (_gcrsGeneratedTime x)
        <&> \y -> x { _gcrsGeneratedTime = y }
{-# INLINE gcrsGeneratedTime #-}

-- | Contains the credential report. The report is Base64-encoded.
gcrsContent :: Lens' GetCredentialReportResponse (Maybe ByteString)
gcrsContent f x =
    f (_gcrsContent x)
        <&> \y -> x { _gcrsContent = y }
{-# INLINE gcrsContent #-}

-- | The format (MIME type) of the credential report.
gcrsReportFormat :: Lens' GetCredentialReportResponse (Maybe ReportFormatType)
gcrsReportFormat f x =
    f (_gcrsReportFormat x)
        <&> \y -> x { _gcrsReportFormat = y }
{-# INLINE gcrsReportFormat #-}

instance FromXML GetCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetCredentialReport where
    type Sv GetCredentialReport = IAM
    type Rs GetCredentialReport = GetCredentialReportResponse

    request = post "GetCredentialReport"
    response _ = xmlResponse
