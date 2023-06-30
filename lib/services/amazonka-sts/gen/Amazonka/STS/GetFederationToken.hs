{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.STS.GetFederationToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) for a federated user.
-- A typical use is in a proxy application that gets temporary security
-- credentials on behalf of distributed applications inside a corporate
-- network. You must call the @GetFederationToken@ operation using the
-- long-term security credentials of an IAM user. As a result, this call is
-- appropriate in contexts where those credentials can be safely stored,
-- usually in a server-based application. For a comparison of
-- @GetFederationToken@ with the other API operations that produce
-- temporary credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the Amazon Web Services STS API operations>
-- in the /IAM User Guide/.
--
-- You can create a mobile-based or browser-based app that can authenticate
-- users using a web identity provider like Login with Amazon, Facebook,
-- Google, or an OpenID Connect-compatible identity provider. In this case,
-- we recommend that you use
-- <http://aws.amazon.com/cognito/ Amazon Cognito> or
-- @AssumeRoleWithWebIdentity@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_assumerolewithwebidentity Federation Through a Web-based Identity Provider>
-- in the /IAM User Guide/.
--
-- You can also call @GetFederationToken@ using the security credentials of
-- an Amazon Web Services account root user, but we do not recommend it.
-- Instead, we recommend that you create an IAM user for the purpose of the
-- proxy application. Then attach a policy to the IAM user that limits
-- federated users to only the actions and resources that they need to
-- access. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html IAM Best Practices>
-- in the /IAM User Guide/.
--
-- __Session duration__
--
-- The temporary credentials are valid for the specified duration, from 900
-- seconds (15 minutes) up to a maximum of 129,600 seconds (36 hours). The
-- default session duration is 43,200 seconds (12 hours). Temporary
-- credentials obtained by using the Amazon Web Services account root user
-- credentials have a maximum duration of 3,600 seconds (1 hour).
--
-- __Permissions__
--
-- You can use the temporary credentials created by @GetFederationToken@ in
-- any Amazon Web Services service except the following:
--
-- -   You cannot call any IAM operations using the CLI or the Amazon Web
--     Services API.
--
-- -   You cannot call any STS operations except @GetCallerIdentity@.
--
-- You must pass an inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policy
-- Amazon Resource Names (ARNs) to use as managed session policies. The
-- plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters.
--
-- Though the session policy parameters are optional, if you do not pass a
-- policy, then the resulting federated user session has no permissions.
-- When you pass session policies, the session permissions are the
-- intersection of the IAM user policies and the session policies that you
-- pass. This gives you a way to further restrict the permissions for a
-- federated user. You cannot use session policies to grant more
-- permissions than those that are defined in the permissions policy of the
-- IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/. For information about using
-- @GetFederationToken@ to create temporary security credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_getfederationtoken GetFederationToken—Federation Through a Custom Identity Broker>.
--
-- You can use the credentials to access a resource that has a
-- resource-based policy. If that policy specifically references the
-- federated user session in the @Principal@ element of the policy, the
-- session has the permissions allowed by the policy. These permissions are
-- granted in addition to the permissions granted by the session policies.
--
-- __Tags__
--
-- (Optional) You can pass tag key-value pairs to your session. These are
-- called session tags. For more information about session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
--
-- You can create a mobile-based or browser-based app that can authenticate
-- users using a web identity provider like Login with Amazon, Facebook,
-- Google, or an OpenID Connect-compatible identity provider. In this case,
-- we recommend that you use
-- <http://aws.amazon.com/cognito/ Amazon Cognito> or
-- @AssumeRoleWithWebIdentity@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_assumerolewithwebidentity Federation Through a Web-based Identity Provider>
-- in the /IAM User Guide/.
--
-- An administrator must grant you the permissions necessary to pass
-- session tags. The administrator can also create granular permissions to
-- allow you to pass only specific session tags. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control>
-- in the /IAM User Guide/.
--
-- Tag key–value pairs are not case sensitive, but case is preserved. This
-- means that you cannot have separate @Department@ and @department@ tag
-- keys. Assume that the user that you are federating has the
-- @Department@=@Marketing@ tag and you pass the @department@=@engineering@
-- session tag. @Department@ and @department@ are not saved as separate
-- tags, and the session tag passed in the request takes precedence over
-- the user tag.
module Amazonka.STS.GetFederationToken
  ( -- * Creating a Request
    GetFederationToken (..),
    newGetFederationToken,

    -- * Request Lenses
    getFederationToken_durationSeconds,
    getFederationToken_policy,
    getFederationToken_policyArns,
    getFederationToken_tags,
    getFederationToken_name,

    -- * Destructuring the Response
    GetFederationTokenResponse (..),
    newGetFederationTokenResponse,

    -- * Response Lenses
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_federatedUser,
    getFederationTokenResponse_packedPolicySize,
    getFederationTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.STS.Types

-- | /See:/ 'newGetFederationToken' smart constructor.
data GetFederationToken = GetFederationToken'
  { -- | The duration, in seconds, that the session should last. Acceptable
    -- durations for federation sessions range from 900 seconds (15 minutes) to
    -- 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the
    -- default. Sessions obtained using Amazon Web Services account root user
    -- credentials are restricted to a maximum of 3,600 seconds (one hour). If
    -- the specified duration is longer than one hour, the session obtained by
    -- using root user credentials defaults to one hour.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | An IAM policy in JSON format that you want to use as an inline session
    -- policy.
    --
    -- You must pass an inline or managed
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
    -- to this operation. You can pass a single JSON policy document to use as
    -- an inline session policy. You can also specify up to 10 managed policy
    -- Amazon Resource Names (ARNs) to use as managed session policies.
    --
    -- This parameter is optional. However, if you do not pass any session
    -- policies, then the resulting federated user session has no permissions.
    --
    -- When you pass session policies, the session permissions are the
    -- intersection of the IAM user policies and the session policies that you
    -- pass. This gives you a way to further restrict the permissions for a
    -- federated user. You cannot use session policies to grant more
    -- permissions than those that are defined in the permissions policy of the
    -- IAM user. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
    -- in the /IAM User Guide/.
    --
    -- The resulting credentials can be used to access a resource that has a
    -- resource-based policy. If that policy specifically references the
    -- federated user session in the @Principal@ element of the policy, the
    -- session has the permissions allowed by the policy. These permissions are
    -- granted in addition to the permissions that are granted by the session
    -- policies.
    --
    -- The plaintext that you use for both inline and managed session policies
    -- can\'t exceed 2,048 characters. The JSON policy characters can be any
    -- ASCII character from the space character to the end of the valid
    -- character list (\\u0020 through \\u00FF). It can also include the tab
    -- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
    --
    -- An Amazon Web Services conversion compresses the passed inline session
    -- policy, managed policy ARNs, and session tags into a packed binary
    -- format that has a separate limit. Your request can fail for this limit
    -- even if your plaintext meets the other requirements. The
    -- @PackedPolicySize@ response element indicates by percentage how close
    -- the policies and tags for your request are to the upper size limit.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you
    -- want to use as a managed session policy. The policies must exist in the
    -- same account as the IAM user that is requesting federated access.
    --
    -- You must pass an inline or managed
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
    -- to this operation. You can pass a single JSON policy document to use as
    -- an inline session policy. You can also specify up to 10 managed policy
    -- Amazon Resource Names (ARNs) to use as managed session policies. The
    -- plaintext that you use for both inline and managed session policies
    -- can\'t exceed 2,048 characters. You can provide up to 10 managed policy
    -- ARNs. For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the Amazon Web Services General Reference.
    --
    -- This parameter is optional. However, if you do not pass any session
    -- policies, then the resulting federated user session has no permissions.
    --
    -- When you pass session policies, the session permissions are the
    -- intersection of the IAM user policies and the session policies that you
    -- pass. This gives you a way to further restrict the permissions for a
    -- federated user. You cannot use session policies to grant more
    -- permissions than those that are defined in the permissions policy of the
    -- IAM user. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
    -- in the /IAM User Guide/.
    --
    -- The resulting credentials can be used to access a resource that has a
    -- resource-based policy. If that policy specifically references the
    -- federated user session in the @Principal@ element of the policy, the
    -- session has the permissions allowed by the policy. These permissions are
    -- granted in addition to the permissions that are granted by the session
    -- policies.
    --
    -- An Amazon Web Services conversion compresses the passed inline session
    -- policy, managed policy ARNs, and session tags into a packed binary
    -- format that has a separate limit. Your request can fail for this limit
    -- even if your plaintext meets the other requirements. The
    -- @PackedPolicySize@ response element indicates by percentage how close
    -- the policies and tags for your request are to the upper size limit.
    policyArns :: Prelude.Maybe [PolicyDescriptorType],
    -- | A list of session tags. Each session tag consists of a key name and an
    -- associated value. For more information about session tags, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. You can pass up to 50 session tags. The
    -- plaintext session tag keys can’t exceed 128 characters and the values
    -- can’t exceed 256 characters. For these and additional limits, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
    -- in the /IAM User Guide/.
    --
    -- An Amazon Web Services conversion compresses the passed inline session
    -- policy, managed policy ARNs, and session tags into a packed binary
    -- format that has a separate limit. Your request can fail for this limit
    -- even if your plaintext meets the other requirements. The
    -- @PackedPolicySize@ response element indicates by percentage how close
    -- the policies and tags for your request are to the upper size limit.
    --
    -- You can pass a session tag with the same key as a tag that is already
    -- attached to the user you are federating. When you do, session tags
    -- override a user tag with the same key.
    --
    -- Tag key–value pairs are not case sensitive, but case is preserved. This
    -- means that you cannot have separate @Department@ and @department@ tag
    -- keys. Assume that the role has the @Department@=@Marketing@ tag and you
    -- pass the @department@=@engineering@ session tag. @Department@ and
    -- @department@ are not saved as separate tags, and the session tag passed
    -- in the request takes precedence over the role tag.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the federated user. The name is used as an identifier for
    -- the temporary security credentials (such as @Bob@). For example, you can
    -- reference the federated user name in a resource-based policy, such as in
    -- an Amazon S3 bucket policy.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFederationToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'getFederationToken_durationSeconds' - The duration, in seconds, that the session should last. Acceptable
-- durations for federation sessions range from 900 seconds (15 minutes) to
-- 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the
-- default. Sessions obtained using Amazon Web Services account root user
-- credentials are restricted to a maximum of 3,600 seconds (one hour). If
-- the specified duration is longer than one hour, the session obtained by
-- using root user credentials defaults to one hour.
--
-- 'policy', 'getFederationToken_policy' - An IAM policy in JSON format that you want to use as an inline session
-- policy.
--
-- You must pass an inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policy
-- Amazon Resource Names (ARNs) to use as managed session policies.
--
-- This parameter is optional. However, if you do not pass any session
-- policies, then the resulting federated user session has no permissions.
--
-- When you pass session policies, the session permissions are the
-- intersection of the IAM user policies and the session policies that you
-- pass. This gives you a way to further restrict the permissions for a
-- federated user. You cannot use session policies to grant more
-- permissions than those that are defined in the permissions policy of the
-- IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The resulting credentials can be used to access a resource that has a
-- resource-based policy. If that policy specifically references the
-- federated user session in the @Principal@ element of the policy, the
-- session has the permissions allowed by the policy. These permissions are
-- granted in addition to the permissions that are granted by the session
-- policies.
--
-- The plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. The JSON policy characters can be any
-- ASCII character from the space character to the end of the valid
-- character list (\\u0020 through \\u00FF). It can also include the tab
-- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- 'policyArns', 'getFederationToken_policyArns' - The Amazon Resource Names (ARNs) of the IAM managed policies that you
-- want to use as a managed session policy. The policies must exist in the
-- same account as the IAM user that is requesting federated access.
--
-- You must pass an inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policy
-- Amazon Resource Names (ARNs) to use as managed session policies. The
-- plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. You can provide up to 10 managed policy
-- ARNs. For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the Amazon Web Services General Reference.
--
-- This parameter is optional. However, if you do not pass any session
-- policies, then the resulting federated user session has no permissions.
--
-- When you pass session policies, the session permissions are the
-- intersection of the IAM user policies and the session policies that you
-- pass. This gives you a way to further restrict the permissions for a
-- federated user. You cannot use session policies to grant more
-- permissions than those that are defined in the permissions policy of the
-- IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The resulting credentials can be used to access a resource that has a
-- resource-based policy. If that policy specifically references the
-- federated user session in the @Principal@ element of the policy, the
-- session has the permissions allowed by the policy. These permissions are
-- granted in addition to the permissions that are granted by the session
-- policies.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- 'tags', 'getFederationToken_tags' - A list of session tags. Each session tag consists of a key name and an
-- associated value. For more information about session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
--
-- This parameter is optional. You can pass up to 50 session tags. The
-- plaintext session tag keys can’t exceed 128 characters and the values
-- can’t exceed 256 characters. For these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- You can pass a session tag with the same key as a tag that is already
-- attached to the user you are federating. When you do, session tags
-- override a user tag with the same key.
--
-- Tag key–value pairs are not case sensitive, but case is preserved. This
-- means that you cannot have separate @Department@ and @department@ tag
-- keys. Assume that the role has the @Department@=@Marketing@ tag and you
-- pass the @department@=@engineering@ session tag. @Department@ and
-- @department@ are not saved as separate tags, and the session tag passed
-- in the request takes precedence over the role tag.
--
-- 'name', 'getFederationToken_name' - The name of the federated user. The name is used as an identifier for
-- the temporary security credentials (such as @Bob@). For example, you can
-- reference the federated user name in a resource-based policy, such as in
-- an Amazon S3 bucket policy.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
newGetFederationToken ::
  -- | 'name'
  Prelude.Text ->
  GetFederationToken
newGetFederationToken pName_ =
  GetFederationToken'
    { durationSeconds =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      policyArns = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The duration, in seconds, that the session should last. Acceptable
-- durations for federation sessions range from 900 seconds (15 minutes) to
-- 129,600 seconds (36 hours), with 43,200 seconds (12 hours) as the
-- default. Sessions obtained using Amazon Web Services account root user
-- credentials are restricted to a maximum of 3,600 seconds (one hour). If
-- the specified duration is longer than one hour, the session obtained by
-- using root user credentials defaults to one hour.
getFederationToken_durationSeconds :: Lens.Lens' GetFederationToken (Prelude.Maybe Prelude.Natural)
getFederationToken_durationSeconds = Lens.lens (\GetFederationToken' {durationSeconds} -> durationSeconds) (\s@GetFederationToken' {} a -> s {durationSeconds = a} :: GetFederationToken)

-- | An IAM policy in JSON format that you want to use as an inline session
-- policy.
--
-- You must pass an inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policy
-- Amazon Resource Names (ARNs) to use as managed session policies.
--
-- This parameter is optional. However, if you do not pass any session
-- policies, then the resulting federated user session has no permissions.
--
-- When you pass session policies, the session permissions are the
-- intersection of the IAM user policies and the session policies that you
-- pass. This gives you a way to further restrict the permissions for a
-- federated user. You cannot use session policies to grant more
-- permissions than those that are defined in the permissions policy of the
-- IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The resulting credentials can be used to access a resource that has a
-- resource-based policy. If that policy specifically references the
-- federated user session in the @Principal@ element of the policy, the
-- session has the permissions allowed by the policy. These permissions are
-- granted in addition to the permissions that are granted by the session
-- policies.
--
-- The plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. The JSON policy characters can be any
-- ASCII character from the space character to the end of the valid
-- character list (\\u0020 through \\u00FF). It can also include the tab
-- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
getFederationToken_policy :: Lens.Lens' GetFederationToken (Prelude.Maybe Prelude.Text)
getFederationToken_policy = Lens.lens (\GetFederationToken' {policy} -> policy) (\s@GetFederationToken' {} a -> s {policy = a} :: GetFederationToken)

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you
-- want to use as a managed session policy. The policies must exist in the
-- same account as the IAM user that is requesting federated access.
--
-- You must pass an inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policy>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policy
-- Amazon Resource Names (ARNs) to use as managed session policies. The
-- plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. You can provide up to 10 managed policy
-- ARNs. For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the Amazon Web Services General Reference.
--
-- This parameter is optional. However, if you do not pass any session
-- policies, then the resulting federated user session has no permissions.
--
-- When you pass session policies, the session permissions are the
-- intersection of the IAM user policies and the session policies that you
-- pass. This gives you a way to further restrict the permissions for a
-- federated user. You cannot use session policies to grant more
-- permissions than those that are defined in the permissions policy of the
-- IAM user. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The resulting credentials can be used to access a resource that has a
-- resource-based policy. If that policy specifically references the
-- federated user session in the @Principal@ element of the policy, the
-- session has the permissions allowed by the policy. These permissions are
-- granted in addition to the permissions that are granted by the session
-- policies.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
getFederationToken_policyArns :: Lens.Lens' GetFederationToken (Prelude.Maybe [PolicyDescriptorType])
getFederationToken_policyArns = Lens.lens (\GetFederationToken' {policyArns} -> policyArns) (\s@GetFederationToken' {} a -> s {policyArns = a} :: GetFederationToken) Prelude.. Lens.mapping Lens.coerced

-- | A list of session tags. Each session tag consists of a key name and an
-- associated value. For more information about session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
--
-- This parameter is optional. You can pass up to 50 session tags. The
-- plaintext session tag keys can’t exceed 128 characters and the values
-- can’t exceed 256 characters. For these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- You can pass a session tag with the same key as a tag that is already
-- attached to the user you are federating. When you do, session tags
-- override a user tag with the same key.
--
-- Tag key–value pairs are not case sensitive, but case is preserved. This
-- means that you cannot have separate @Department@ and @department@ tag
-- keys. Assume that the role has the @Department@=@Marketing@ tag and you
-- pass the @department@=@engineering@ session tag. @Department@ and
-- @department@ are not saved as separate tags, and the session tag passed
-- in the request takes precedence over the role tag.
getFederationToken_tags :: Lens.Lens' GetFederationToken (Prelude.Maybe [Tag])
getFederationToken_tags = Lens.lens (\GetFederationToken' {tags} -> tags) (\s@GetFederationToken' {} a -> s {tags = a} :: GetFederationToken) Prelude.. Lens.mapping Lens.coerced

-- | The name of the federated user. The name is used as an identifier for
-- the temporary security credentials (such as @Bob@). For example, you can
-- reference the federated user name in a resource-based policy, such as in
-- an Amazon S3 bucket policy.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
getFederationToken_name :: Lens.Lens' GetFederationToken Prelude.Text
getFederationToken_name = Lens.lens (\GetFederationToken' {name} -> name) (\s@GetFederationToken' {} a -> s {name = a} :: GetFederationToken)

instance Core.AWSRequest GetFederationToken where
  type
    AWSResponse GetFederationToken =
      GetFederationTokenResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetFederationTokenResult"
      ( \s h x ->
          GetFederationTokenResponse'
            Prelude.<$> (x Data..@? "Credentials")
            Prelude.<*> (x Data..@? "FederatedUser")
            Prelude.<*> (x Data..@? "PackedPolicySize")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFederationToken where
  hashWithSalt _salt GetFederationToken' {..} =
    _salt
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyArns
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetFederationToken where
  rnf GetFederationToken' {..} =
    Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf policyArns
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetFederationToken where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFederationToken where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFederationToken where
  toQuery GetFederationToken' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetFederationToken" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-06-15" :: Prelude.ByteString),
        "DurationSeconds" Data.=: durationSeconds,
        "Policy" Data.=: policy,
        "PolicyArns"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> policyArns),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "Name" Data.=: name
      ]

-- | Contains the response to a successful GetFederationToken request,
-- including temporary Amazon Web Services credentials that can be used to
-- make Amazon Web Services requests.
--
-- /See:/ 'newGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { -- | The temporary security credentials, which include an access key ID, a
    -- secret access key, and a security (or session) token.
    --
    -- The size of the security token that STS API operations return is not
    -- fixed. We strongly recommend that you make no assumptions about the
    -- maximum size.
    credentials :: Prelude.Maybe Core.AuthEnv,
    -- | Identifiers for the federated user associated with the credentials (such
    -- as @arn:aws:sts::123456789012:federated-user\/Bob@ or
    -- @123456789012:Bob@). You can use the federated user\'s ARN in your
    -- resource-based policies, such as an Amazon S3 bucket policy.
    federatedUser :: Prelude.Maybe FederatedUser,
    -- | A percentage value that indicates the packed size of the session
    -- policies and session tags combined passed in the request. The request
    -- fails if the packed size is greater than 100 percent, which means the
    -- policies and tags exceeded the allowed space.
    packedPolicySize :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFederationTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getFederationTokenResponse_credentials' - The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
--
-- 'federatedUser', 'getFederationTokenResponse_federatedUser' - Identifiers for the federated user associated with the credentials (such
-- as @arn:aws:sts::123456789012:federated-user\/Bob@ or
-- @123456789012:Bob@). You can use the federated user\'s ARN in your
-- resource-based policies, such as an Amazon S3 bucket policy.
--
-- 'packedPolicySize', 'getFederationTokenResponse_packedPolicySize' - A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
--
-- 'httpStatus', 'getFederationTokenResponse_httpStatus' - The response's http status code.
newGetFederationTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFederationTokenResponse
newGetFederationTokenResponse pHttpStatus_ =
  GetFederationTokenResponse'
    { credentials =
        Prelude.Nothing,
      federatedUser = Prelude.Nothing,
      packedPolicySize = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
getFederationTokenResponse_credentials :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Core.AuthEnv)
getFederationTokenResponse_credentials = Lens.lens (\GetFederationTokenResponse' {credentials} -> credentials) (\s@GetFederationTokenResponse' {} a -> s {credentials = a} :: GetFederationTokenResponse)

-- | Identifiers for the federated user associated with the credentials (such
-- as @arn:aws:sts::123456789012:federated-user\/Bob@ or
-- @123456789012:Bob@). You can use the federated user\'s ARN in your
-- resource-based policies, such as an Amazon S3 bucket policy.
getFederationTokenResponse_federatedUser :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe FederatedUser)
getFederationTokenResponse_federatedUser = Lens.lens (\GetFederationTokenResponse' {federatedUser} -> federatedUser) (\s@GetFederationTokenResponse' {} a -> s {federatedUser = a} :: GetFederationTokenResponse)

-- | A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
getFederationTokenResponse_packedPolicySize :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Prelude.Natural)
getFederationTokenResponse_packedPolicySize = Lens.lens (\GetFederationTokenResponse' {packedPolicySize} -> packedPolicySize) (\s@GetFederationTokenResponse' {} a -> s {packedPolicySize = a} :: GetFederationTokenResponse)

-- | The response's http status code.
getFederationTokenResponse_httpStatus :: Lens.Lens' GetFederationTokenResponse Prelude.Int
getFederationTokenResponse_httpStatus = Lens.lens (\GetFederationTokenResponse' {httpStatus} -> httpStatus) (\s@GetFederationTokenResponse' {} a -> s {httpStatus = a} :: GetFederationTokenResponse)

instance Prelude.NFData GetFederationTokenResponse where
  rnf GetFederationTokenResponse' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf federatedUser
      `Prelude.seq` Prelude.rnf packedPolicySize
      `Prelude.seq` Prelude.rnf httpStatus
