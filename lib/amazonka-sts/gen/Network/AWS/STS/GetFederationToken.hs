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
-- Module      : Network.AWS.STS.GetFederationToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials (consisting of an access key ID, a secret access key, and a security token) for a federated user. A typical use is in a proxy application that gets temporary security credentials on behalf of distributed applications inside a corporate network. You must call the @GetFederationToken@ operation using the long-term security credentials of an IAM user. As a result, this call is appropriate in contexts where those credentials can be safely stored, usually in a server-based application. For a comparison of @GetFederationToken@ with the other API operations that produce temporary credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations> in the /IAM User Guide/ .
--
--
-- You can also call @GetFederationToken@ using the security credentials of an AWS account root user, but we do not recommend it. Instead, we recommend that you create an IAM user for the purpose of the proxy application. Then attach a policy to the IAM user that limits federated users to only the actions and resources that they need to access. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html IAM Best Practices> in the /IAM User Guide/ .
--
-- __Session duration__
--
-- The temporary credentials are valid for the specified duration, from 900 seconds (15 minutes) up to a maximum of 129,600 seconds (36 hours). The default session duration is 43,200 seconds (12 hours). Temporary credentials that are obtained by using AWS account root user credentials have a maximum duration of 3,600 seconds (1 hour).
--
-- __Permissions__
--
-- You can use the temporary credentials created by @GetFederationToken@ in any AWS service except the following:
--
--     * You cannot call any IAM operations using the AWS CLI or the AWS API.
--
--     * You cannot call any STS operations except @GetCallerIdentity@ .
--
--
--
-- You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters.
--
-- Though the session policy parameters are optional, if you do not pass a policy, then the resulting federated user session has no permissions. When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . For information about using @GetFederationToken@ to create temporary security credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getfederationtoken GetFederationToken—Federation Through a Custom Identity Broker> .
--
-- You can use the credentials to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions granted by the session policies.
--
-- __Tags__
--
-- (Optional) You can pass tag key-value pairs to your session. These are called session tags. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
--
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
--
-- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the user that you are federating has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the user tag.
module Network.AWS.STS.GetFederationToken
  ( -- * Creating a Request
    getFederationToken,
    GetFederationToken,

    -- * Request Lenses
    gftPolicyARNs,
    gftDurationSeconds,
    gftPolicy,
    gftTags,
    gftName,

    -- * Destructuring the Response
    getFederationTokenResponse,
    GetFederationTokenResponse,

    -- * Response Lenses
    gftrsPackedPolicySize,
    gftrsCredentials,
    gftrsFederatedUser,
    gftrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types

-- | /See:/ 'getFederationToken' smart constructor.
data GetFederationToken = GetFederationToken'
  { _gftPolicyARNs ::
      !(Maybe [PolicyDescriptorType]),
    _gftDurationSeconds :: !(Maybe Nat),
    _gftPolicy :: !(Maybe Text),
    _gftTags :: !(Maybe [Tag]),
    _gftName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFederationToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gftPolicyARNs' - The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as a managed session policy. The policies must exist in the same account as the IAM user that is requesting federated access. You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. You can provide up to 10 managed policy ARNs. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference. This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions. When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies.
--
-- * 'gftDurationSeconds' - The duration, in seconds, that the session should last. Acceptable durations for federation sessions range from 900 seconds (15 minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the default. Sessions obtained using AWS account root user credentials are restricted to a maximum of 3,600 seconds (one hour). If the specified duration is longer than one hour, the session obtained by using root user credentials defaults to one hour.
--
-- * 'gftPolicy' - An IAM policy in JSON format that you want to use as an inline session policy. You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions. When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- * 'gftTags' - A list of session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ . This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ . You can pass a session tag with the same key as a tag that is already attached to the user you are federating. When you do, session tags override a user tag with the same key.  Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
--
-- * 'gftName' - The name of the federated user. The name is used as an identifier for the temporary security credentials (such as @Bob@ ). For example, you can reference the federated user name in a resource-based policy, such as in an Amazon S3 bucket policy. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
getFederationToken ::
  -- | 'gftName'
  Text ->
  GetFederationToken
getFederationToken pName_ =
  GetFederationToken'
    { _gftPolicyARNs = Nothing,
      _gftDurationSeconds = Nothing,
      _gftPolicy = Nothing,
      _gftTags = Nothing,
      _gftName = pName_
    }

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as a managed session policy. The policies must exist in the same account as the IAM user that is requesting federated access. You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. You can provide up to 10 managed policy ARNs. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference. This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions. When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies.
gftPolicyARNs :: Lens' GetFederationToken [PolicyDescriptorType]
gftPolicyARNs = lens _gftPolicyARNs (\s a -> s {_gftPolicyARNs = a}) . _Default . _Coerce

-- | The duration, in seconds, that the session should last. Acceptable durations for federation sessions range from 900 seconds (15 minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the default. Sessions obtained using AWS account root user credentials are restricted to a maximum of 3,600 seconds (one hour). If the specified duration is longer than one hour, the session obtained by using root user credentials defaults to one hour.
gftDurationSeconds :: Lens' GetFederationToken (Maybe Natural)
gftDurationSeconds = lens _gftDurationSeconds (\s a -> s {_gftDurationSeconds = a}) . mapping _Nat

-- | An IAM policy in JSON format that you want to use as an inline session policy. You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions. When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
gftPolicy :: Lens' GetFederationToken (Maybe Text)
gftPolicy = lens _gftPolicy (\s a -> s {_gftPolicy = a})

-- | A list of session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ . This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ . You can pass a session tag with the same key as a tag that is already attached to the user you are federating. When you do, session tags override a user tag with the same key.  Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
gftTags :: Lens' GetFederationToken [Tag]
gftTags = lens _gftTags (\s a -> s {_gftTags = a}) . _Default . _Coerce

-- | The name of the federated user. The name is used as an identifier for the temporary security credentials (such as @Bob@ ). For example, you can reference the federated user name in a resource-based policy, such as in an Amazon S3 bucket policy. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
gftName :: Lens' GetFederationToken Text
gftName = lens _gftName (\s a -> s {_gftName = a})

instance AWSRequest GetFederationToken where
  type Rs GetFederationToken = GetFederationTokenResponse
  request = postQuery sts
  response =
    receiveXMLWrapper
      "GetFederationTokenResult"
      ( \s h x ->
          GetFederationTokenResponse'
            <$> (x .@? "PackedPolicySize")
            <*> (x .@? "Credentials")
            <*> (x .@? "FederatedUser")
            <*> (pure (fromEnum s))
      )

instance Hashable GetFederationToken

instance NFData GetFederationToken

instance ToHeaders GetFederationToken where
  toHeaders = const mempty

instance ToPath GetFederationToken where
  toPath = const "/"

instance ToQuery GetFederationToken where
  toQuery GetFederationToken' {..} =
    mconcat
      [ "Action" =: ("GetFederationToken" :: ByteString),
        "Version" =: ("2011-06-15" :: ByteString),
        "PolicyArns" =: toQuery (toQueryList "member" <$> _gftPolicyARNs),
        "DurationSeconds" =: _gftDurationSeconds,
        "Policy" =: _gftPolicy,
        "Tags" =: toQuery (toQueryList "member" <$> _gftTags),
        "Name" =: _gftName
      ]

-- | Contains the response to a successful 'GetFederationToken' request, including temporary AWS credentials that can be used to make AWS requests.
--
--
--
-- /See:/ 'getFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { _gftrsPackedPolicySize ::
      !(Maybe Nat),
    _gftrsCredentials :: !(Maybe AuthEnv),
    _gftrsFederatedUser ::
      !(Maybe FederatedUser),
    _gftrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFederationTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gftrsPackedPolicySize' - A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- * 'gftrsCredentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- * 'gftrsFederatedUser' - Identifiers for the federated user associated with the credentials (such as @arn:aws:sts::123456789012:federated-user/Bob@ or @123456789012:Bob@ ). You can use the federated user's ARN in your resource-based policies, such as an Amazon S3 bucket policy.
--
-- * 'gftrsResponseStatus' - -- | The response status code.
getFederationTokenResponse ::
  -- | 'gftrsResponseStatus'
  Int ->
  GetFederationTokenResponse
getFederationTokenResponse pResponseStatus_ =
  GetFederationTokenResponse'
    { _gftrsPackedPolicySize = Nothing,
      _gftrsCredentials = Nothing,
      _gftrsFederatedUser = Nothing,
      _gftrsResponseStatus = pResponseStatus_
    }

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
gftrsPackedPolicySize :: Lens' GetFederationTokenResponse (Maybe Natural)
gftrsPackedPolicySize = lens _gftrsPackedPolicySize (\s a -> s {_gftrsPackedPolicySize = a}) . mapping _Nat

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
gftrsCredentials :: Lens' GetFederationTokenResponse (Maybe AuthEnv)
gftrsCredentials = lens _gftrsCredentials (\s a -> s {_gftrsCredentials = a})

-- | Identifiers for the federated user associated with the credentials (such as @arn:aws:sts::123456789012:federated-user/Bob@ or @123456789012:Bob@ ). You can use the federated user's ARN in your resource-based policies, such as an Amazon S3 bucket policy.
gftrsFederatedUser :: Lens' GetFederationTokenResponse (Maybe FederatedUser)
gftrsFederatedUser = lens _gftrsFederatedUser (\s a -> s {_gftrsFederatedUser = a})

-- | -- | The response status code.
gftrsResponseStatus :: Lens' GetFederationTokenResponse Int
gftrsResponseStatus = lens _gftrsResponseStatus (\s a -> s {_gftrsResponseStatus = a})

instance NFData GetFederationTokenResponse
