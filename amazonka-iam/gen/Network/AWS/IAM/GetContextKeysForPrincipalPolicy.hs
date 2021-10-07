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
-- Module      : Network.AWS.IAM.GetContextKeysForPrincipalPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all of the context keys referenced in all the IAM
-- policies that are attached to the specified IAM entity. The entity can
-- be an IAM user, group, or role. If you specify a user, then the request
-- also includes all of the policies attached to groups that the user is a
-- member of.
--
-- You can optionally include a list of one or more additional policies,
-- specified as strings. If you want to include /only/ a list of policies
-- by string, use GetContextKeysForCustomPolicy instead.
--
-- __Note:__ This operation discloses information about the permissions
-- granted to other users. If you do not want users to see other user\'s
-- permissions, then consider allowing them to use
-- GetContextKeysForCustomPolicy instead.
--
-- Context keys are variables maintained by Amazon Web Services and its
-- services that provide details about the context of an API query request.
-- Context keys can be evaluated by testing against a value in an IAM
-- policy. Use GetContextKeysForPrincipalPolicy to understand what key
-- names and values you must supply when you call SimulatePrincipalPolicy.
module Network.AWS.IAM.GetContextKeysForPrincipalPolicy
  ( -- * Creating a Request
    GetContextKeysForPrincipalPolicy (..),
    newGetContextKeysForPrincipalPolicy,

    -- * Request Lenses
    getContextKeysForPrincipalPolicy_policyInputList,
    getContextKeysForPrincipalPolicy_policySourceArn,

    -- * Destructuring the Response
    GetContextKeysForPolicyResponse (..),
    newGetContextKeysForPolicyResponse,

    -- * Response Lenses
    getContextKeysForPolicyResponse_contextKeyNames,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContextKeysForPrincipalPolicy' smart constructor.
data GetContextKeysForPrincipalPolicy = GetContextKeysForPrincipalPolicy'
  { -- | An optional list of additional policies for which you want the list of
    -- context keys that are referenced.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    policyInputList :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of a user, group, or role whose policies contain the context
    -- keys that you want listed. If you specify a user, the list includes
    -- context keys that are found in all policies that are attached to the
    -- user. The list also includes all groups that the user is a member of. If
    -- you pick a group or a role, then it includes only those context keys
    -- that are found in policies attached to that entity. Note that all
    -- parameters are shown in unencoded form here for clarity, but must be URL
    -- encoded to be included as a part of a real HTML request.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policySourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContextKeysForPrincipalPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyInputList', 'getContextKeysForPrincipalPolicy_policyInputList' - An optional list of additional policies for which you want the list of
-- context keys that are referenced.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- 'policySourceArn', 'getContextKeysForPrincipalPolicy_policySourceArn' - The ARN of a user, group, or role whose policies contain the context
-- keys that you want listed. If you specify a user, the list includes
-- context keys that are found in all policies that are attached to the
-- user. The list also includes all groups that the user is a member of. If
-- you pick a group or a role, then it includes only those context keys
-- that are found in policies attached to that entity. Note that all
-- parameters are shown in unencoded form here for clarity, but must be URL
-- encoded to be included as a part of a real HTML request.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newGetContextKeysForPrincipalPolicy ::
  -- | 'policySourceArn'
  Prelude.Text ->
  GetContextKeysForPrincipalPolicy
newGetContextKeysForPrincipalPolicy pPolicySourceArn_ =
  GetContextKeysForPrincipalPolicy'
    { policyInputList =
        Prelude.Nothing,
      policySourceArn = pPolicySourceArn_
    }

-- | An optional list of additional policies for which you want the list of
-- context keys that are referenced.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
getContextKeysForPrincipalPolicy_policyInputList :: Lens.Lens' GetContextKeysForPrincipalPolicy (Prelude.Maybe [Prelude.Text])
getContextKeysForPrincipalPolicy_policyInputList = Lens.lens (\GetContextKeysForPrincipalPolicy' {policyInputList} -> policyInputList) (\s@GetContextKeysForPrincipalPolicy' {} a -> s {policyInputList = a} :: GetContextKeysForPrincipalPolicy) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of a user, group, or role whose policies contain the context
-- keys that you want listed. If you specify a user, the list includes
-- context keys that are found in all policies that are attached to the
-- user. The list also includes all groups that the user is a member of. If
-- you pick a group or a role, then it includes only those context keys
-- that are found in policies attached to that entity. Note that all
-- parameters are shown in unencoded form here for clarity, but must be URL
-- encoded to be included as a part of a real HTML request.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
getContextKeysForPrincipalPolicy_policySourceArn :: Lens.Lens' GetContextKeysForPrincipalPolicy Prelude.Text
getContextKeysForPrincipalPolicy_policySourceArn = Lens.lens (\GetContextKeysForPrincipalPolicy' {policySourceArn} -> policySourceArn) (\s@GetContextKeysForPrincipalPolicy' {} a -> s {policySourceArn = a} :: GetContextKeysForPrincipalPolicy)

instance
  Core.AWSRequest
    GetContextKeysForPrincipalPolicy
  where
  type
    AWSResponse GetContextKeysForPrincipalPolicy =
      GetContextKeysForPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetContextKeysForPrincipalPolicyResult"
      (\s h x -> Core.parseXML x)

instance
  Prelude.Hashable
    GetContextKeysForPrincipalPolicy

instance
  Prelude.NFData
    GetContextKeysForPrincipalPolicy

instance
  Core.ToHeaders
    GetContextKeysForPrincipalPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetContextKeysForPrincipalPolicy where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetContextKeysForPrincipalPolicy
  where
  toQuery GetContextKeysForPrincipalPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetContextKeysForPrincipalPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "PolicyInputList"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> policyInputList
            ),
        "PolicySourceArn" Core.=: policySourceArn
      ]
