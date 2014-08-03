{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GenerateCredentialReport
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a credential report for the AWS account. For more information
-- about the credential report, see Getting Credential Reports in the Using
-- IAM guide.
module Network.AWS.IAM.V2010_05_08.GenerateCredentialReport where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data GenerateCredentialReport = GenerateCredentialReport
    deriving (Eq, Show, Generic)

makeLenses ''GenerateCredentialReport

instance ToQuery GenerateCredentialReport where
    toQuery = genericToQuery def

data GenerateCredentialReportResponse = GenerateCredentialReportResponse
    { _gcrsDescription :: Maybe Text
      -- ^ Information about the credential report.
    , _gcrsState :: Maybe ReportStateType
      -- ^ Information about the state of a credential report.
    } deriving (Generic)

makeLenses ''GenerateCredentialReportResponse

instance FromXML GenerateCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GenerateCredentialReport where
    type Sv GenerateCredentialReport = IAM
    type Rs GenerateCredentialReport = GenerateCredentialReportResponse

    request = post "GenerateCredentialReport"
    response _ = xmlResponse
