{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GenerateServiceLastAccessedDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report that includes details about when an IAM resource (user, group, role, or policy) was last used in an attempt to access AWS services. Recent activity usually appears within four hours. IAM reports activity for the last 365 days, or less if your Region began supporting this feature within the last year. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> .
--
--
-- /Important:/ The service last accessed data includes all attempts to access an AWS API, not just the successful ones. This includes all attempts that were made using the AWS Management Console, the AWS API through any of the SDKs, or any of the command line tools. An unexpected entry in the service last accessed data does not mean that your account has been compromised, because the request might have been denied. Refer to your CloudTrail logs as the authoritative source for information about all API calls and whether they were successful or denied access. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html Logging IAM Events with CloudTrail> in the /IAM User Guide/ .
--
-- The @GenerateServiceLastAccessedDetails@ operation returns a @JobId@ . Use this parameter in the following operations to retrieve the following details from your report:
--
--     * 'GetServiceLastAccessedDetails' – Use this operation for users, groups, roles, or policies to list every AWS service that the resource could access using permissions policies. For each service, the response includes information about the most recent access attempt.
--
-- The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be used by the same role within a session, or by the same user when used to call @GetServiceLastAccessedDetail@ .
--
--     * 'GetServiceLastAccessedDetailsWithEntities' – Use this operation for groups and policies to list information about the associated entities (users or roles) that attempted to access a specific AWS service.
--
--
--
-- To check the status of the @GenerateServiceLastAccessedDetails@ request, use the @JobId@ parameter in the same operations and test the @JobStatus@ response parameter.
--
-- For additional information about the permissions policies that allow an identity (user, group, or role) to access specific services, use the 'ListPoliciesGrantingServiceAccess' operation.
--
-- For more information about service and action last accessed data, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing Permissions Using Service Last Accessed Data> in the /IAM User Guide/ .
module Network.AWS.IAM.GenerateServiceLastAccessedDetails
  ( -- * Creating a Request
    generateServiceLastAccessedDetails,
    GenerateServiceLastAccessedDetails,

    -- * Request Lenses
    gsladGranularity,
    gsladARN,

    -- * Destructuring the Response
    generateServiceLastAccessedDetailsResponse,
    GenerateServiceLastAccessedDetailsResponse,

    -- * Response Lenses
    gsladsrsJobId,
    gsladsrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateServiceLastAccessedDetails' smart constructor.
data GenerateServiceLastAccessedDetails = GenerateServiceLastAccessedDetails'
  { _gsladGranularity ::
      !( Maybe
           AccessAdvisorUsageGranularityType
       ),
    _gsladARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateServiceLastAccessedDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsladGranularity' - The level of detail that you want to generate. You can specify whether you want to generate information about the last attempt to access services or actions. If you specify service-level granularity, this operation generates only service data. If you specify action-level granularity, it generates service and action data. If you don't include this optional parameter, the operation generates service data.
--
-- * 'gsladARN' - The ARN of the IAM resource (user, group, role, or managed policy) used to generate information about when the resource was last used in an attempt to access an AWS service.
generateServiceLastAccessedDetails ::
  -- | 'gsladARN'
  Text ->
  GenerateServiceLastAccessedDetails
generateServiceLastAccessedDetails pARN_ =
  GenerateServiceLastAccessedDetails'
    { _gsladGranularity = Nothing,
      _gsladARN = pARN_
    }

-- | The level of detail that you want to generate. You can specify whether you want to generate information about the last attempt to access services or actions. If you specify service-level granularity, this operation generates only service data. If you specify action-level granularity, it generates service and action data. If you don't include this optional parameter, the operation generates service data.
gsladGranularity :: Lens' GenerateServiceLastAccessedDetails (Maybe AccessAdvisorUsageGranularityType)
gsladGranularity = lens _gsladGranularity (\s a -> s {_gsladGranularity = a})

-- | The ARN of the IAM resource (user, group, role, or managed policy) used to generate information about when the resource was last used in an attempt to access an AWS service.
gsladARN :: Lens' GenerateServiceLastAccessedDetails Text
gsladARN = lens _gsladARN (\s a -> s {_gsladARN = a})

instance AWSRequest GenerateServiceLastAccessedDetails where
  type
    Rs GenerateServiceLastAccessedDetails =
      GenerateServiceLastAccessedDetailsResponse
  request = postQuery iam
  response =
    receiveXMLWrapper
      "GenerateServiceLastAccessedDetailsResult"
      ( \s h x ->
          GenerateServiceLastAccessedDetailsResponse'
            <$> (x .@? "JobId") <*> (pure (fromEnum s))
      )

instance Hashable GenerateServiceLastAccessedDetails

instance NFData GenerateServiceLastAccessedDetails

instance ToHeaders GenerateServiceLastAccessedDetails where
  toHeaders = const mempty

instance ToPath GenerateServiceLastAccessedDetails where
  toPath = const "/"

instance ToQuery GenerateServiceLastAccessedDetails where
  toQuery GenerateServiceLastAccessedDetails' {..} =
    mconcat
      [ "Action" =: ("GenerateServiceLastAccessedDetails" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "Granularity" =: _gsladGranularity,
        "Arn" =: _gsladARN
      ]

-- | /See:/ 'generateServiceLastAccessedDetailsResponse' smart constructor.
data GenerateServiceLastAccessedDetailsResponse = GenerateServiceLastAccessedDetailsResponse'
  { _gsladsrsJobId ::
      !( Maybe
           Text
       ),
    _gsladsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GenerateServiceLastAccessedDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsladsrsJobId' - The @JobId@ that you can use in the 'GetServiceLastAccessedDetails' or 'GetServiceLastAccessedDetailsWithEntities' operations. The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be used by the same role within a session, or by the same user when used to call @GetServiceLastAccessedDetail@ .
--
-- * 'gsladsrsResponseStatus' - -- | The response status code.
generateServiceLastAccessedDetailsResponse ::
  -- | 'gsladsrsResponseStatus'
  Int ->
  GenerateServiceLastAccessedDetailsResponse
generateServiceLastAccessedDetailsResponse pResponseStatus_ =
  GenerateServiceLastAccessedDetailsResponse'
    { _gsladsrsJobId =
        Nothing,
      _gsladsrsResponseStatus = pResponseStatus_
    }

-- | The @JobId@ that you can use in the 'GetServiceLastAccessedDetails' or 'GetServiceLastAccessedDetailsWithEntities' operations. The @JobId@ returned by @GenerateServiceLastAccessedDetail@ must be used by the same role within a session, or by the same user when used to call @GetServiceLastAccessedDetail@ .
gsladsrsJobId :: Lens' GenerateServiceLastAccessedDetailsResponse (Maybe Text)
gsladsrsJobId = lens _gsladsrsJobId (\s a -> s {_gsladsrsJobId = a})

-- | -- | The response status code.
gsladsrsResponseStatus :: Lens' GenerateServiceLastAccessedDetailsResponse Int
gsladsrsResponseStatus = lens _gsladsrsResponseStatus (\s a -> s {_gsladsrsResponseStatus = a})

instance NFData GenerateServiceLastAccessedDetailsResponse
