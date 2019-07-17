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
-- Module      : Network.AWS.IAM.GenerateServiceLastAccessedDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a request for a report that includes details about when an IAM resource (user, group, role, or policy) was last used in an attempt to access AWS services. Recent activity usually appears within four hours. IAM reports activity for the last 365 days, or less if your region began supporting this feature within the last year. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> .
--
--
-- /Important:/ The service last accessed data includes all attempts to access an AWS API, not just the successful ones. This includes all attempts that were made using the AWS Management Console, the AWS API through any of the SDKs, or any of the command line tools. An unexpected entry in the service last accessed data does not mean that your account has been compromised, because the request might have been denied. Refer to your CloudTrail logs as the authoritative source for information about all API calls and whether they were successful or denied access. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM Events with CloudTrail> in the /IAM User Guide/ .
--
-- The @GenerateServiceLastAccessedDetails@ operation returns a @JobId@ . Use this parameter in the following operations to retrieve the following details from your report:
--
--     * 'GetServiceLastAccessedDetails' – Use this operation for users, groups, roles, or policies to list every AWS service that the resource could access using permissions policies. For each service, the response includes information about the most recent access attempt.
--
--     * 'GetServiceLastAccessedDetailsWithEntities' – Use this operation for groups and policies to list information about the associated entities (users or roles) that attempted to access a specific AWS service.
--
--
--
-- To check the status of the @GenerateServiceLastAccessedDetails@ request, use the @JobId@ parameter in the same operations and test the @JobStatus@ response parameter.
--
-- For additional information about the permissions policies that allow an identity (user, group, or role) to access specific services, use the 'ListPoliciesGrantingServiceAccess' operation.
--
-- For more information about service last accessed data, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Policy Scope by Viewing User Activity> in the /IAM User Guide/ .
--
module Network.AWS.IAM.GenerateServiceLastAccessedDetails
    (
    -- * Creating a Request
      generateServiceLastAccessedDetails
    , GenerateServiceLastAccessedDetails
    -- * Request Lenses
    , gsladARN

    -- * Destructuring the Response
    , generateServiceLastAccessedDetailsResponse
    , GenerateServiceLastAccessedDetailsResponse
    -- * Response Lenses
    , gsladsrsJobId
    , gsladsrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateServiceLastAccessedDetails' smart constructor.
newtype GenerateServiceLastAccessedDetails = GenerateServiceLastAccessedDetails'
  { _gsladARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateServiceLastAccessedDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsladARN' - The ARN of the IAM resource (user, group, role, or managed policy) used to generate information about when the resource was last used in an attempt to access an AWS service.
generateServiceLastAccessedDetails
    :: Text -- ^ 'gsladARN'
    -> GenerateServiceLastAccessedDetails
generateServiceLastAccessedDetails pARN_ =
  GenerateServiceLastAccessedDetails' {_gsladARN = pARN_}


-- | The ARN of the IAM resource (user, group, role, or managed policy) used to generate information about when the resource was last used in an attempt to access an AWS service.
gsladARN :: Lens' GenerateServiceLastAccessedDetails Text
gsladARN = lens _gsladARN (\ s a -> s{_gsladARN = a})

instance AWSRequest
           GenerateServiceLastAccessedDetails
         where
        type Rs GenerateServiceLastAccessedDetails =
             GenerateServiceLastAccessedDetailsResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "GenerateServiceLastAccessedDetailsResult"
              (\ s h x ->
                 GenerateServiceLastAccessedDetailsResponse' <$>
                   (x .@? "JobId") <*> (pure (fromEnum s)))

instance Hashable GenerateServiceLastAccessedDetails
         where

instance NFData GenerateServiceLastAccessedDetails
         where

instance ToHeaders GenerateServiceLastAccessedDetails
         where
        toHeaders = const mempty

instance ToPath GenerateServiceLastAccessedDetails
         where
        toPath = const "/"

instance ToQuery GenerateServiceLastAccessedDetails
         where
        toQuery GenerateServiceLastAccessedDetails'{..}
          = mconcat
              ["Action" =:
                 ("GenerateServiceLastAccessedDetails" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Arn" =: _gsladARN]

-- | /See:/ 'generateServiceLastAccessedDetailsResponse' smart constructor.
data GenerateServiceLastAccessedDetailsResponse = GenerateServiceLastAccessedDetailsResponse'
  { _gsladsrsJobId          :: !(Maybe Text)
  , _gsladsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateServiceLastAccessedDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsladsrsJobId' - The job ID that you can use in the 'GetServiceLastAccessedDetails' or 'GetServiceLastAccessedDetailsWithEntities' operations.
--
-- * 'gsladsrsResponseStatus' - -- | The response status code.
generateServiceLastAccessedDetailsResponse
    :: Int -- ^ 'gsladsrsResponseStatus'
    -> GenerateServiceLastAccessedDetailsResponse
generateServiceLastAccessedDetailsResponse pResponseStatus_ =
  GenerateServiceLastAccessedDetailsResponse'
    {_gsladsrsJobId = Nothing, _gsladsrsResponseStatus = pResponseStatus_}


-- | The job ID that you can use in the 'GetServiceLastAccessedDetails' or 'GetServiceLastAccessedDetailsWithEntities' operations.
gsladsrsJobId :: Lens' GenerateServiceLastAccessedDetailsResponse (Maybe Text)
gsladsrsJobId = lens _gsladsrsJobId (\ s a -> s{_gsladsrsJobId = a})

-- | -- | The response status code.
gsladsrsResponseStatus :: Lens' GenerateServiceLastAccessedDetailsResponse Int
gsladsrsResponseStatus = lens _gsladsrsResponseStatus (\ s a -> s{_gsladsrsResponseStatus = a})

instance NFData
           GenerateServiceLastAccessedDetailsResponse
         where
