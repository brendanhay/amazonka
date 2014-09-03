{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.GenerateCredentialReport
    (
    -- * Request
      GenerateCredentialReport
    -- ** Request constructor
    , generateCredentialReport
    -- * Response
    , GenerateCredentialReportResponse
    -- ** Response lenses
    , gcrrDescription
    , gcrrState
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GenerateCredentialReport' request.
generateCredentialReport :: GenerateCredentialReport
generateCredentialReport = GenerateCredentialReport

data GenerateCredentialReport = GenerateCredentialReport
    deriving (Eq, Show, Generic)

instance ToQuery GenerateCredentialReport where
    toQuery = genericQuery def

data GenerateCredentialReportResponse = GenerateCredentialReportResponse
    { _gcrrDescription :: Maybe Text
      -- ^ Information about the credential report.
    , _gcrrState :: Maybe ReportStateType
      -- ^ Information about the state of a credential report.
    } deriving (Show, Generic)

-- | Information about the credential report.
gcrrDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GenerateCredentialReportResponse
    -> f GenerateCredentialReportResponse
gcrrDescription f x =
    (\y -> x { _gcrrDescription = y })
       <$> f (_gcrrDescription x)
{-# INLINE gcrrDescription #-}

-- | Information about the state of a credential report.
gcrrState
    :: Functor f
    => (Maybe ReportStateType
    -> f (Maybe ReportStateType))
    -> GenerateCredentialReportResponse
    -> f GenerateCredentialReportResponse
gcrrState f x =
    (\y -> x { _gcrrState = y })
       <$> f (_gcrrState x)
{-# INLINE gcrrState #-}

instance FromXML GenerateCredentialReportResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GenerateCredentialReport where
    type Sv GenerateCredentialReport = IAM
    type Rs GenerateCredentialReport = GenerateCredentialReportResponse

    request = post "GenerateCredentialReport"
    response _ = xmlResponse
