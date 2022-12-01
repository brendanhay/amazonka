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
-- Module      : Amazonka.IAM.GetContextKeysForCustomPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all of the context keys referenced in the input policies.
-- The policies are supplied as a list of one or more strings. To get the
-- context keys from policies associated with an IAM user, group, or role,
-- use GetContextKeysForPrincipalPolicy.
--
-- Context keys are variables maintained by Amazon Web Services and its
-- services that provide details about the context of an API query request.
-- Context keys can be evaluated by testing against a value specified in an
-- IAM policy. Use @GetContextKeysForCustomPolicy@ to understand what key
-- names and values you must supply when you call SimulateCustomPolicy.
-- Note that all parameters are shown in unencoded form here for clarity
-- but must be URL encoded to be included as a part of a real HTML request.
module Amazonka.IAM.GetContextKeysForCustomPolicy
  ( -- * Creating a Request
    GetContextKeysForCustomPolicy (..),
    newGetContextKeysForCustomPolicy,

    -- * Request Lenses
    getContextKeysForCustomPolicy_policyInputList,

    -- * Destructuring the Response
    GetContextKeysForPolicyResponse (..),
    newGetContextKeysForPolicyResponse,

    -- * Response Lenses
    getContextKeysForPolicyResponse_contextKeyNames,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContextKeysForCustomPolicy' smart constructor.
data GetContextKeysForCustomPolicy = GetContextKeysForCustomPolicy'
  { -- | A list of policies for which you want the list of context keys
    -- referenced in those policies. Each document is specified as a string
    -- containing the complete, valid JSON text of an IAM policy.
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
    policyInputList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContextKeysForCustomPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyInputList', 'getContextKeysForCustomPolicy_policyInputList' - A list of policies for which you want the list of context keys
-- referenced in those policies. Each document is specified as a string
-- containing the complete, valid JSON text of an IAM policy.
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
newGetContextKeysForCustomPolicy ::
  GetContextKeysForCustomPolicy
newGetContextKeysForCustomPolicy =
  GetContextKeysForCustomPolicy'
    { policyInputList =
        Prelude.mempty
    }

-- | A list of policies for which you want the list of context keys
-- referenced in those policies. Each document is specified as a string
-- containing the complete, valid JSON text of an IAM policy.
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
getContextKeysForCustomPolicy_policyInputList :: Lens.Lens' GetContextKeysForCustomPolicy [Prelude.Text]
getContextKeysForCustomPolicy_policyInputList = Lens.lens (\GetContextKeysForCustomPolicy' {policyInputList} -> policyInputList) (\s@GetContextKeysForCustomPolicy' {} a -> s {policyInputList = a} :: GetContextKeysForCustomPolicy) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetContextKeysForCustomPolicy
  where
  type
    AWSResponse GetContextKeysForCustomPolicy =
      GetContextKeysForPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetContextKeysForCustomPolicyResult"
      (\s h x -> Core.parseXML x)

instance
  Prelude.Hashable
    GetContextKeysForCustomPolicy
  where
  hashWithSalt _salt GetContextKeysForCustomPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyInputList

instance Prelude.NFData GetContextKeysForCustomPolicy where
  rnf GetContextKeysForCustomPolicy' {..} =
    Prelude.rnf policyInputList

instance Core.ToHeaders GetContextKeysForCustomPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetContextKeysForCustomPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery GetContextKeysForCustomPolicy where
  toQuery GetContextKeysForCustomPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetContextKeysForCustomPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "PolicyInputList"
          Core.=: Core.toQueryList "member" policyInputList
      ]
