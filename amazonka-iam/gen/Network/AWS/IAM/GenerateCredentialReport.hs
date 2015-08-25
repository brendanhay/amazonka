{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GenerateCredentialReport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a credential report for the AWS account. For more information
-- about the credential report, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GenerateCredentialReport.html AWS API Reference> for GenerateCredentialReport.
module Network.AWS.IAM.GenerateCredentialReport
    (
    -- * Creating a Request
      generateCredentialReport
    , GenerateCredentialReport

    -- * Destructuring the Response
    , generateCredentialReportResponse
    , GenerateCredentialReportResponse
    -- * Response Lenses
    , gcrrsState
    , gcrrsDescription
    , gcrrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateCredentialReport' smart constructor.
data GenerateCredentialReport =
    GenerateCredentialReport'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateCredentialReport' with the minimum fields required to make a request.
--
generateCredentialReport
    :: GenerateCredentialReport
generateCredentialReport = GenerateCredentialReport'

instance AWSRequest GenerateCredentialReport where
        type Rs GenerateCredentialReport =
             GenerateCredentialReportResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "GenerateCredentialReportResult"
              (\ s h x ->
                 GenerateCredentialReportResponse' <$>
                   (x .@? "State") <*> (x .@? "Description") <*>
                     (pure (fromEnum s)))

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

-- | Contains the response to a successful GenerateCredentialReport request.
--
-- /See:/ 'generateCredentialReportResponse' smart constructor.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'
    { _gcrrsState       :: !(Maybe ReportStateType)
    , _gcrrsDescription :: !(Maybe Text)
    , _gcrrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateCredentialReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrrsState'
--
-- * 'gcrrsDescription'
--
-- * 'gcrrsStatus'
generateCredentialReportResponse
    :: Int -- ^ 'gcrrsStatus'
    -> GenerateCredentialReportResponse
generateCredentialReportResponse pStatus_ =
    GenerateCredentialReportResponse'
    { _gcrrsState = Nothing
    , _gcrrsDescription = Nothing
    , _gcrrsStatus = pStatus_
    }

-- | Information about the state of the credential report.
gcrrsState :: Lens' GenerateCredentialReportResponse (Maybe ReportStateType)
gcrrsState = lens _gcrrsState (\ s a -> s{_gcrrsState = a});

-- | Information about the credential report.
gcrrsDescription :: Lens' GenerateCredentialReportResponse (Maybe Text)
gcrrsDescription = lens _gcrrsDescription (\ s a -> s{_gcrrsDescription = a});

-- | The response status code.
gcrrsStatus :: Lens' GenerateCredentialReportResponse Int
gcrrsStatus = lens _gcrrsStatus (\ s a -> s{_gcrrsStatus = a});
