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
-- For more information about how permissions work, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_getfederationtoken.html Permissions for GetFederationToken> . For information about using @GetFederationToken@ to create temporary security credentials, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getfederationtoken GetFederationToken—Federation Through a Custom Identity Broker> .
--
module Network.AWS.STS.GetFederationToken
    (
    -- * Creating a Request
      getFederationToken
    , GetFederationToken
    -- * Request Lenses
    , gftDurationSeconds
    , gftPolicy
    , gftName

    -- * Destructuring the Response
    , getFederationTokenResponse
    , GetFederationTokenResponse
    -- * Response Lenses
    , gftrsPackedPolicySize
    , gftrsCredentials
    , gftrsFederatedUser
    , gftrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types
import Network.AWS.STS.Types.Product

-- | /See:/ 'getFederationToken' smart constructor.
data GetFederationToken = GetFederationToken'
  { _gftDurationSeconds :: !(Maybe Nat)
  , _gftPolicy          :: !(Maybe Text)
  , _gftName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFederationToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gftDurationSeconds' - The duration, in seconds, that the session should last. Acceptable durations for federation sessions range from 900 seconds (15 minutes) to 129600 seconds (36 hours), with 43200 seconds (12 hours) as the default. Sessions obtained using AWS account (root) credentials are restricted to a maximum of 3600 seconds (one hour). If the specified duration is longer than one hour, the session obtained by using AWS account (root) credentials defaults to one hour.
--
-- * 'gftPolicy' - An IAM policy in JSON format that is passed with the @GetFederationToken@ call and evaluated along with the policy or policies that are attached to the IAM user whose credentials are used to call @GetFederationToken@ . The passed policy is used to scope down the permissions that are available to the IAM user, by allowing only a subset of the permissions that are granted to the IAM user. The passed policy cannot grant more permissions than those granted to the IAM user. The final permissions for the federated user are the most restrictive set based on the intersection of the passed policy and the IAM user policy. If you do not pass a policy, the resulting temporary security credentials have no effective permissions. The only exception is when the temporary security credentials are used to access a resource that has a resource-based policy that specifically allows the federated user to access the resource. The format for this parameter, as described by its regex pattern, is a string of characters up to 2048 characters in length. The characters can be any ASCII character from the space character to the end of the valid character list (\u0020-\u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters. For more information about how permissions work, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_getfederationtoken.html Permissions for GetFederationToken> .
--
-- * 'gftName' - The name of the federated user. The name is used as an identifier for the temporary security credentials (such as @Bob@ ). For example, you can reference the federated user name in a resource-based policy, such as in an Amazon S3 bucket policy. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
getFederationToken
    :: Text -- ^ 'gftName'
    -> GetFederationToken
getFederationToken pName_ =
  GetFederationToken'
    {_gftDurationSeconds = Nothing, _gftPolicy = Nothing, _gftName = pName_}


-- | The duration, in seconds, that the session should last. Acceptable durations for federation sessions range from 900 seconds (15 minutes) to 129600 seconds (36 hours), with 43200 seconds (12 hours) as the default. Sessions obtained using AWS account (root) credentials are restricted to a maximum of 3600 seconds (one hour). If the specified duration is longer than one hour, the session obtained by using AWS account (root) credentials defaults to one hour.
gftDurationSeconds :: Lens' GetFederationToken (Maybe Natural)
gftDurationSeconds = lens _gftDurationSeconds (\ s a -> s{_gftDurationSeconds = a}) . mapping _Nat

-- | An IAM policy in JSON format that is passed with the @GetFederationToken@ call and evaluated along with the policy or policies that are attached to the IAM user whose credentials are used to call @GetFederationToken@ . The passed policy is used to scope down the permissions that are available to the IAM user, by allowing only a subset of the permissions that are granted to the IAM user. The passed policy cannot grant more permissions than those granted to the IAM user. The final permissions for the federated user are the most restrictive set based on the intersection of the passed policy and the IAM user policy. If you do not pass a policy, the resulting temporary security credentials have no effective permissions. The only exception is when the temporary security credentials are used to access a resource that has a resource-based policy that specifically allows the federated user to access the resource. The format for this parameter, as described by its regex pattern, is a string of characters up to 2048 characters in length. The characters can be any ASCII character from the space character to the end of the valid character list (\u0020-\u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters. For more information about how permissions work, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_getfederationtoken.html Permissions for GetFederationToken> .
gftPolicy :: Lens' GetFederationToken (Maybe Text)
gftPolicy = lens _gftPolicy (\ s a -> s{_gftPolicy = a})

-- | The name of the federated user. The name is used as an identifier for the temporary security credentials (such as @Bob@ ). For example, you can reference the federated user name in a resource-based policy, such as in an Amazon S3 bucket policy. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
gftName :: Lens' GetFederationToken Text
gftName = lens _gftName (\ s a -> s{_gftName = a})

instance AWSRequest GetFederationToken where
        type Rs GetFederationToken =
             GetFederationTokenResponse
        request = postQuery sts
        response
          = receiveXMLWrapper "GetFederationTokenResult"
              (\ s h x ->
                 GetFederationTokenResponse' <$>
                   (x .@? "PackedPolicySize") <*> (x .@? "Credentials")
                     <*> (x .@? "FederatedUser")
                     <*> (pure (fromEnum s)))

instance Hashable GetFederationToken where

instance NFData GetFederationToken where

instance ToHeaders GetFederationToken where
        toHeaders = const mempty

instance ToPath GetFederationToken where
        toPath = const "/"

instance ToQuery GetFederationToken where
        toQuery GetFederationToken'{..}
          = mconcat
              ["Action" =: ("GetFederationToken" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "DurationSeconds" =: _gftDurationSeconds,
               "Policy" =: _gftPolicy, "Name" =: _gftName]

-- | Contains the response to a successful 'GetFederationToken' request, including temporary AWS credentials that can be used to make AWS requests.
--
--
--
-- /See:/ 'getFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { _gftrsPackedPolicySize :: !(Maybe Nat)
  , _gftrsCredentials      :: !(Maybe AuthEnv)
  , _gftrsFederatedUser    :: !(Maybe FederatedUser)
  , _gftrsResponseStatus   :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFederationTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gftrsPackedPolicySize' - A percentage value indicating the size of the policy in packed form. The service rejects policies for which the packed size is greater than 100 percent of the allowed value.
--
-- * 'gftrsCredentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token. __Note:__ The size of the security token that STS APIs return is not fixed. We strongly recommend that you make no assumptions about the maximum size. As of this writing, the typical size is less than 4096 bytes, but that can vary. Also, future updates to AWS might require larger sizes.
--
-- * 'gftrsFederatedUser' - Identifiers for the federated user associated with the credentials (such as @arn:aws:sts::123456789012:federated-user/Bob@ or @123456789012:Bob@ ). You can use the federated user's ARN in your resource-based policies, such as an Amazon S3 bucket policy.
--
-- * 'gftrsResponseStatus' - -- | The response status code.
getFederationTokenResponse
    :: Int -- ^ 'gftrsResponseStatus'
    -> GetFederationTokenResponse
getFederationTokenResponse pResponseStatus_ =
  GetFederationTokenResponse'
    { _gftrsPackedPolicySize = Nothing
    , _gftrsCredentials = Nothing
    , _gftrsFederatedUser = Nothing
    , _gftrsResponseStatus = pResponseStatus_
    }


-- | A percentage value indicating the size of the policy in packed form. The service rejects policies for which the packed size is greater than 100 percent of the allowed value.
gftrsPackedPolicySize :: Lens' GetFederationTokenResponse (Maybe Natural)
gftrsPackedPolicySize = lens _gftrsPackedPolicySize (\ s a -> s{_gftrsPackedPolicySize = a}) . mapping _Nat

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token. __Note:__ The size of the security token that STS APIs return is not fixed. We strongly recommend that you make no assumptions about the maximum size. As of this writing, the typical size is less than 4096 bytes, but that can vary. Also, future updates to AWS might require larger sizes.
gftrsCredentials :: Lens' GetFederationTokenResponse (Maybe AuthEnv)
gftrsCredentials = lens _gftrsCredentials (\ s a -> s{_gftrsCredentials = a})

-- | Identifiers for the federated user associated with the credentials (such as @arn:aws:sts::123456789012:federated-user/Bob@ or @123456789012:Bob@ ). You can use the federated user's ARN in your resource-based policies, such as an Amazon S3 bucket policy.
gftrsFederatedUser :: Lens' GetFederationTokenResponse (Maybe FederatedUser)
gftrsFederatedUser = lens _gftrsFederatedUser (\ s a -> s{_gftrsFederatedUser = a})

-- | -- | The response status code.
gftrsResponseStatus :: Lens' GetFederationTokenResponse Int
gftrsResponseStatus = lens _gftrsResponseStatus (\ s a -> s{_gftrsResponseStatus = a})

instance NFData GetFederationTokenResponse where
