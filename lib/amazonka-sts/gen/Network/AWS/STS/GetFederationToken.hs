{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- You can also call @GetFederationToken@ using the security credentials of an AWS account root user, but we do not recommend it. Instead, we recommend that you create an IAM user for the purpose of the proxy application. Then attach a policy to the IAM user that limits federated users to only the actions and resources that they need to access. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html IAM Best Practices> in the /IAM User Guide/ .
-- __Session duration__
-- The temporary credentials are valid for the specified duration, from 900 seconds (15 minutes) up to a maximum of 129,600 seconds (36 hours). The default session duration is 43,200 seconds (12 hours). Temporary credentials that are obtained by using AWS account root user credentials have a maximum duration of 3,600 seconds (1 hour).
-- __Permissions__
-- You can use the temporary credentials created by @GetFederationToken@ in any AWS service except the following:
--
--     * You cannot call any IAM operations using the AWS CLI or the AWS API.
--
--
--     * You cannot call any STS operations except @GetCallerIdentity@ .
--
--
-- You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters.
-- Though the session policy parameters are optional, if you do not pass a policy, then the resulting federated user session has no permissions. When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . For information about using @GetFederationToken@ to create temporary security credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getfederationtoken GetFederationToken—Federation Through a Custom Identity Broker> .
-- You can use the credentials to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions granted by the session policies.
-- __Tags__
-- (Optional) You can pass tag key-value pairs to your session. These are called session tags. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
-- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the user that you are federating has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the user tag.
module Network.AWS.STS.GetFederationToken
  ( -- * Creating a request
    GetFederationToken (..),
    mkGetFederationToken,

    -- ** Request lenses
    gftName,
    gftDurationSeconds,
    gftPolicy,
    gftPolicyArns,
    gftTags,

    -- * Destructuring the response
    GetFederationTokenResponse (..),
    mkGetFederationTokenResponse,

    -- ** Response lenses
    gftrrsCredentials,
    gftrrsFederatedUser,
    gftrrsPackedPolicySize,
    gftrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.STS.Types as Types

-- | /See:/ 'mkGetFederationToken' smart constructor.
data GetFederationToken = GetFederationToken'
  { -- | The name of the federated user. The name is used as an identifier for the temporary security credentials (such as @Bob@ ). For example, you can reference the federated user name in a resource-based policy, such as in an Amazon S3 bucket policy.
    --
    -- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
    name :: Types.Name,
    -- | The duration, in seconds, that the session should last. Acceptable durations for federation sessions range from 900 seconds (15 minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the default. Sessions obtained using AWS account root user credentials are restricted to a maximum of 3,600 seconds (one hour). If the specified duration is longer than one hour, the session obtained by using root user credentials defaults to one hour.
    durationSeconds :: Core.Maybe Core.Natural,
    -- | An IAM policy in JSON format that you want to use as an inline session policy.
    --
    -- You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies.
    -- This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions.
    -- When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    -- The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies.
    -- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
    policy :: Core.Maybe Types.Policy,
    -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as a managed session policy. The policies must exist in the same account as the IAM user that is requesting federated access.
    --
    -- You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. You can provide up to 10 managed policy ARNs. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
    -- This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions.
    -- When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    -- The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies.
    policyArns :: Core.Maybe [Types.PolicyDescriptorType],
    -- | A list of session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
    --
    -- This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
    -- You can pass a session tag with the same key as a tag that is already attached to the user you are federating. When you do, session tags override a user tag with the same key.
    -- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFederationToken' value with any optional fields omitted.
mkGetFederationToken ::
  -- | 'name'
  Types.Name ->
  GetFederationToken
mkGetFederationToken name =
  GetFederationToken'
    { name,
      durationSeconds = Core.Nothing,
      policy = Core.Nothing,
      policyArns = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the federated user. The name is used as an identifier for the temporary security credentials (such as @Bob@ ). For example, you can reference the federated user name in a resource-based policy, such as in an Amazon S3 bucket policy.
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftName :: Lens.Lens' GetFederationToken Types.Name
gftName = Lens.field @"name"
{-# DEPRECATED gftName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The duration, in seconds, that the session should last. Acceptable durations for federation sessions range from 900 seconds (15 minutes) to 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the default. Sessions obtained using AWS account root user credentials are restricted to a maximum of 3,600 seconds (one hour). If the specified duration is longer than one hour, the session obtained by using root user credentials defaults to one hour.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftDurationSeconds :: Lens.Lens' GetFederationToken (Core.Maybe Core.Natural)
gftDurationSeconds = Lens.field @"durationSeconds"
{-# DEPRECATED gftDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | An IAM policy in JSON format that you want to use as an inline session policy.
--
-- You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies.
-- This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions.
-- When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies.
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftPolicy :: Lens.Lens' GetFederationToken (Core.Maybe Types.Policy)
gftPolicy = Lens.field @"policy"
{-# DEPRECATED gftPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as a managed session policy. The policies must exist in the same account as the IAM user that is requesting federated access.
--
-- You must pass an inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. You can provide up to 10 managed policy ARNs. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- This parameter is optional. However, if you do not pass any session policies, then the resulting federated user session has no permissions.
-- When you pass session policies, the session permissions are the intersection of the IAM user policies and the session policies that you pass. This gives you a way to further restrict the permissions for a federated user. You cannot use session policies to grant more permissions than those that are defined in the permissions policy of the IAM user. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The resulting credentials can be used to access a resource that has a resource-based policy. If that policy specifically references the federated user session in the @Principal@ element of the policy, the session has the permissions allowed by the policy. These permissions are granted in addition to the permissions that are granted by the session policies.
--
-- /Note:/ Consider using 'policyArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftPolicyArns :: Lens.Lens' GetFederationToken (Core.Maybe [Types.PolicyDescriptorType])
gftPolicyArns = Lens.field @"policyArns"
{-# DEPRECATED gftPolicyArns "Use generic-lens or generic-optics with 'policyArns' instead." #-}

-- | A list of session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
--
-- This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
-- You can pass a session tag with the same key as a tag that is already attached to the user you are federating. When you do, session tags override a user tag with the same key.
-- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftTags :: Lens.Lens' GetFederationToken (Core.Maybe [Types.Tag])
gftTags = Lens.field @"tags"
{-# DEPRECATED gftTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest GetFederationToken where
  type Rs GetFederationToken = GetFederationTokenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetFederationToken")
                Core.<> (Core.pure ("Version", "2011-06-15"))
                Core.<> (Core.toQueryValue "Name" name)
                Core.<> (Core.toQueryValue "DurationSeconds" Core.<$> durationSeconds)
                Core.<> (Core.toQueryValue "Policy" Core.<$> policy)
                Core.<> ( Core.toQueryValue
                            "PolicyArns"
                            (Core.toQueryList "member" Core.<$> policyArns)
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetFederationTokenResult"
      ( \s h x ->
          GetFederationTokenResponse'
            Core.<$> (x Core..@? "Credentials")
            Core.<*> (x Core..@? "FederatedUser")
            Core.<*> (x Core..@? "PackedPolicySize")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetFederationToken' request, including temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'mkGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { -- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
    credentials :: Core.Maybe Types.AuthEnv,
    -- | Identifiers for the federated user associated with the credentials (such as @arn:aws:sts::123456789012:federated-user/Bob@ or @123456789012:Bob@ ). You can use the federated user's ARN in your resource-based policies, such as an Amazon S3 bucket policy.
    federatedUser :: Core.Maybe Types.FederatedUser,
    -- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
    packedPolicySize :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFederationTokenResponse' value with any optional fields omitted.
mkGetFederationTokenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFederationTokenResponse
mkGetFederationTokenResponse responseStatus =
  GetFederationTokenResponse'
    { credentials = Core.Nothing,
      federatedUser = Core.Nothing,
      packedPolicySize = Core.Nothing,
      responseStatus
    }

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsCredentials :: Lens.Lens' GetFederationTokenResponse (Core.Maybe Types.AuthEnv)
gftrrsCredentials = Lens.field @"credentials"
{-# DEPRECATED gftrrsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | Identifiers for the federated user associated with the credentials (such as @arn:aws:sts::123456789012:federated-user/Bob@ or @123456789012:Bob@ ). You can use the federated user's ARN in your resource-based policies, such as an Amazon S3 bucket policy.
--
-- /Note:/ Consider using 'federatedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsFederatedUser :: Lens.Lens' GetFederationTokenResponse (Core.Maybe Types.FederatedUser)
gftrrsFederatedUser = Lens.field @"federatedUser"
{-# DEPRECATED gftrrsFederatedUser "Use generic-lens or generic-optics with 'federatedUser' instead." #-}

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- /Note:/ Consider using 'packedPolicySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsPackedPolicySize :: Lens.Lens' GetFederationTokenResponse (Core.Maybe Core.Natural)
gftrrsPackedPolicySize = Lens.field @"packedPolicySize"
{-# DEPRECATED gftrrsPackedPolicySize "Use generic-lens or generic-optics with 'packedPolicySize' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsResponseStatus :: Lens.Lens' GetFederationTokenResponse Core.Int
gftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
