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

-- Module      : Network.AWS.IAM.GenerateCredentialReport
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
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GenerateCredentialReport.html>
module Network.AWS.IAM.GenerateCredentialReport
    (
    -- * Request
      GenerateCredentialReport
    -- ** Request constructor
    , generateCredentialReport

    -- * Response
    , GenerateCredentialReportResponse
    -- ** Response constructor
    , generateCredentialReportResponse
    -- ** Response lenses
    , gcrrDescription
    , gcrrState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GenerateCredentialReport = GenerateCredentialReport
    deriving (Eq, Ord, Show, Generic)

-- | 'GenerateCredentialReport' constructor.
generateCredentialReport :: GenerateCredentialReport
generateCredentialReport = GenerateCredentialReport

data GenerateCredentialReportResponse = GenerateCredentialReportResponse
    { _gcrrDescription :: Maybe Text
    , _gcrrState       :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'GenerateCredentialReportResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrrDescription' @::@ 'Maybe' 'Text'
--
-- * 'gcrrState' @::@ 'Maybe' 'Text'
--
generateCredentialReportResponse :: GenerateCredentialReportResponse
generateCredentialReportResponse = GenerateCredentialReportResponse
    { _gcrrState       = Nothing
    , _gcrrDescription = Nothing
    }

-- | Information about the credential report.
gcrrDescription :: Lens' GenerateCredentialReportResponse (Maybe Text)
gcrrDescription = lens _gcrrDescription (\s a -> s { _gcrrDescription = a })

-- | Information about the state of a credential report.
gcrrState :: Lens' GenerateCredentialReportResponse (Maybe Text)
gcrrState = lens _gcrrState (\s a -> s { _gcrrState = a })

instance ToPath GenerateCredentialReport where
    toPath = const "/"

instance ToQuery GenerateCredentialReport where
    toQuery = const mempty

instance ToHeaders GenerateCredentialReport

instance AWSRequest GenerateCredentialReport where
    type Sv GenerateCredentialReport = IAM
    type Rs GenerateCredentialReport = GenerateCredentialReportResponse

    request  = post "GenerateCredentialReport"
    response = xmlResponse

instance FromXML GenerateCredentialReportResponse where
    parseXML = withElement "GenerateCredentialReportResult" $ \x -> GenerateCredentialReportResponse
        <$> x .@? "Description"
        <*> x .@? "State"


Some kind of operator / class to check the types whether to continue?
