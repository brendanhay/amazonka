{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- IAM guide.
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

data GetCredentialReport = GetCredentialReport
    deriving (Eq, Ord, Show, Generic)

-- | 'GetCredentialReport' constructor.
getCredentialReport :: GetCredentialReport
getCredentialReport = GetCredentialReport
instance ToQuery GetCredentialReport

instance ToPath GetCredentialReport where
    toPath = const "/"

data GetCredentialReportResponse = GetCredentialReportResponse
    { _gcrrContent       :: Maybe Base64
    , _gcrrGeneratedTime :: Maybe RFC822
    , _gcrrReportFormat  :: Maybe Text
    } (Eq, Show, Generic)

-- | 'GetCredentialReportResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrrContent' @::@ 'Maybe' 'Base64'
--
-- * 'gcrrGeneratedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gcrrReportFormat' @::@ 'Maybe' 'Text'
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

-- | The time and date when the credential report was created, in ISO 8601
-- date-time format.
gcrrGeneratedTime :: Lens' GetCredentialReportResponse (Maybe UTCTime)
gcrrGeneratedTime =
    lens _gcrrGeneratedTime (\s a -> s { _gcrrGeneratedTime = a })
        . mapping _Time

-- | The format (MIME type) of the credential report.
gcrrReportFormat :: Lens' GetCredentialReportResponse (Maybe Text)
gcrrReportFormat = lens _gcrrReportFormat (\s a -> s { _gcrrReportFormat = a })

instance FromXML GetCredentialReportResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetCredentialReportResponse"

instance AWSRequest GetCredentialReport where
    type Sv GetCredentialReport = IAM
    type Rs GetCredentialReport = GetCredentialReportResponse

    request  = post "GetCredentialReport"
    response = xmlResponse $ \h x -> GetCredentialReportResponse
        <$> x %| "Content"
        <*> x %| "GeneratedTime"
        <*> x %| "ReportFormat"
