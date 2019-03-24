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
-- Module      : Network.AWS.STS.GetFederationToken
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials (consisting of an access key ID, a secret access key, and a security token) for a federated user. A typical use is in a proxy application that gets temporary security credentials on behalf of distributed applications inside a corporate network. Because you must call the @GetFederationToken@ action using the long-term security credentials of an IAM user, this call is appropriate in contexts where those credentials can be safely stored, usually in a server-based application. For a comparison of @GetFederationToken@ with the other APIs that produce temporary credentials, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS APIs> in the /IAM User Guide/ .
--
--
-- The @GetFederationToken@ action must be called by using the long-term AWS security credentials of an IAM user. You can also call @GetFederationToken@ using the security credentials of an AWS root account, but we do not recommended it. Instead, we recommend that you create an IAM user for the purpose of the proxy application and then attach a policy to the IAM user that limits federated users to only the actions and resources that they need access to. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html IAM Best Practices> in the /IAM User Guide/ .
--
-- The temporary security credentials that are obtained by using the long-term credentials of an IAM user are valid for the specified duration, from 900 seconds (15 minutes) up to a maximium of 129600 seconds (36 hours). The default is 43200 seconds (12 hours). Temporary credentials that are obtained by using AWS root account credentials have a maximum duration of 3600 seconds (1 hour).
--
-- The temporary security credentials created by @GetFederationToken@ can be used to make API calls to any AWS service with the following exceptions:
--
--     * You cannot use these credentials to call any IAM APIs.
--
--     * You cannot call any STS APIs except @GetCallerIdentity@ .
--
--
--
-- __Permissions__
--
-- The permissions for the temporary security credentials returned by @GetFederationToken@ are determined by a combination of the following:
--
--     * The policy or policies that are attached to the IAM user whose credentials are used to call @GetFederationToken@ .
--
--     * The policy that is passed as a parameter in the call.
--
--
--
-- The passed policy is attached to the temporary security credentials that result from the @GetFederationToken@ API call--that is, to the /federated user/ . When the federated user makes an AWS request, AWS evaluates the policy attached to the federated user in combination with the policy or policies attached to the IAM user whose credentials were used to call @GetFederationToken@ . AWS allows the federated user's request only when both the federated user /__and__ / the IAM user are explicitly allowed to perform the requested action. The passed policy cannot grant more permissions than those that are defined in the IAM user policy.
--
-- A typical use case is that the permissions of the IAM user whose credentials are used to call @GetFederationToken@ are designed to allow access to all the actions and resources that any federated user will need. Then, for individual users, you pass a policy to the operation that scopes down the permissions to a level that's appropriate to that individual user, using a policy that allows only a subset of permissions that are granted to the IAM user.
--
-- If you do not pass a policy, the resulting temporary security credentials have no effective permissions. The only exception is when the temporary security credentials are used to access a resource that has a resource-based policy that specifically allows the federated user to access the resource.
--
-- For more information about how permissions work, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_getfederationtoken.html Permissions for GetFederationToken> . For information about using @GetFederationToken@ to create temporary security credentials, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getfederationtoken GetFederationToken
