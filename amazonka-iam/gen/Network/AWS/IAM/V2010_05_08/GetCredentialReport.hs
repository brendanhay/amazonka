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
gcrsGeneratedTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetCredentialReportResponse
    -> f GetCredentialReportResponse
gcrsGeneratedTime f x =
    (\y -> x { _gcrsGeneratedTime = y })
       <$> f (_gcrsGeneratedTime x)
{-# INLINE gcrsGeneratedTime #-}

-- | Contains the credential report. The report is Base64-encoded.
gcrsContent
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> GetCredentialReportResponse
    -> f GetCredentialReportResponse
gcrsContent f x =
    (\y -> x { _gcrsContent = y })
       <$> f (_gcrsContent x)
{-# INLINE gcrsContent #-}

-- | The format (MIME type) of the credential report.
gcrsReportFormat
    :: Functor f
    => (Maybe ReportFormatType
    -> f (Maybe ReportFormatType))
    -> GetCredentialReportResponse
    -> f GetCredentialReportResponse
gcrsReportFormat f x =
    (\y -> x { _gcrsReportFormat = y })
       <$> f (_gcrsReportFormat x)
{-# INLINE gcrsReportFormat #-}

instance FromXML GetCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetCredentialReport where
    type Sv GetCredentialReport = IAM
    type Rs GetCredentialReport = GetCredentialReportResponse

    request = post "GetCredentialReport"
    response _ = xmlResponse
