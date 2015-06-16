{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GenerateCredentialReport
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Generates a credential report for the AWS account. For more information
-- about the credential report, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports>
-- in the /Using IAM/ guide.
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
    , gcrrState
    , gcrrDescription
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'generateCredentialReport' smart constructor.
data GenerateCredentialReport = GenerateCredentialReport' deriving (Eq, Read, Show)

-- | 'GenerateCredentialReport' smart constructor.
generateCredentialReport :: GenerateCredentialReport
generateCredentialReport = GenerateCredentialReport';

instance AWSRequest GenerateCredentialReport where
        type Sv GenerateCredentialReport = IAM
        type Rs GenerateCredentialReport =
             GenerateCredentialReportResponse
        request = post
        response
          = receiveXMLWrapper "GenerateCredentialReportResult"
              (\ s h x ->
                 GenerateCredentialReportResponse' <$>
                   (x .@? "State") <*> (x .@? "Description"))

instance ToHeaders GenerateCredentialReport where
        toHeaders = const mempty

instance ToPath GenerateCredentialReport where
        toPath = const "/"

instance ToQuery GenerateCredentialReport where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("GenerateCredentialReport" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | /See:/ 'generateCredentialReportResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrrState'
--
-- * 'gcrrDescription'
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'{_gcrrState :: Maybe ReportStateType, _gcrrDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GenerateCredentialReportResponse' smart constructor.
generateCredentialReportResponse :: GenerateCredentialReportResponse
generateCredentialReportResponse = GenerateCredentialReportResponse'{_gcrrState = Nothing, _gcrrDescription = Nothing};

-- | Information about the state of the credential report.
gcrrState :: Lens' GenerateCredentialReportResponse (Maybe ReportStateType)
gcrrState = lens _gcrrState (\ s a -> s{_gcrrState = a});

-- | Information about the credential report.
gcrrDescription :: Lens' GenerateCredentialReportResponse (Maybe Text)
gcrrDescription = lens _gcrrDescription (\ s a -> s{_gcrrDescription = a});
