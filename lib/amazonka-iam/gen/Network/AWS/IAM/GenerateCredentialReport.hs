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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a credential report for the AWS account. For more information about the credential report, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
--
--
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
    , gcrrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateCredentialReport' smart constructor.
data GenerateCredentialReport =
  GenerateCredentialReport'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateCredentialReport' with the minimum fields required to make a request.
--
generateCredentialReport
    :: GenerateCredentialReport
generateCredentialReport = GenerateCredentialReport'


instance AWSRequest GenerateCredentialReport where
        type Rs GenerateCredentialReport =
             GenerateCredentialReportResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GenerateCredentialReportResult"
              (\ s h x ->
                 GenerateCredentialReportResponse' <$>
                   (x .@? "State") <*> (x .@? "Description") <*>
                     (pure (fromEnum s)))

instance Hashable GenerateCredentialReport where

instance NFData GenerateCredentialReport where

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

-- | Contains the response to a successful 'GenerateCredentialReport' request.
--
--
--
-- /See:/ 'generateCredentialReportResponse' smart constructor.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'
  { _gcrrsState          :: !(Maybe ReportStateType)
  , _gcrrsDescription    :: !(Maybe Text)
  , _gcrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateCredentialReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrrsState' - Information about the state of the credential report.
--
-- * 'gcrrsDescription' - Information about the credential report.
--
-- * 'gcrrsResponseStatus' - -- | The response status code.
generateCredentialReportResponse
    :: Int -- ^ 'gcrrsResponseStatus'
    -> GenerateCredentialReportResponse
generateCredentialReportResponse pResponseStatus_ =
  GenerateCredentialReportResponse'
    { _gcrrsState = Nothing
    , _gcrrsDescription = Nothing
    , _gcrrsResponseStatus = pResponseStatus_
    }


-- | Information about the state of the credential report.
gcrrsState :: Lens' GenerateCredentialReportResponse (Maybe ReportStateType)
gcrrsState = lens _gcrrsState (\ s a -> s{_gcrrsState = a})

-- | Information about the credential report.
gcrrsDescription :: Lens' GenerateCredentialReportResponse (Maybe Text)
gcrrsDescription = lens _gcrrsDescription (\ s a -> s{_gcrrsDescription = a})

-- | -- | The response status code.
gcrrsResponseStatus :: Lens' GenerateCredentialReportResponse Int
gcrrsResponseStatus = lens _gcrrsResponseStatus (\ s a -> s{_gcrrsResponseStatus = a})

instance NFData GenerateCredentialReportResponse
         where
