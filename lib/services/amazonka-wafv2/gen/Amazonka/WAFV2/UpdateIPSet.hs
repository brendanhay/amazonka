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
-- Module      : Amazonka.WAFV2.UpdateIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified IPSet.
--
-- This operation completely replaces the mutable specifications that you
-- already have for the IP set with the ones that you provide to this call.
-- To modify the IP set, retrieve it by calling GetIPSet, update the
-- settings as needed, and then provide the complete IP set specification
-- to this call.
--
-- When you make changes to web ACLs or web ACL components, like rules and
-- rule groups, WAF propagates the changes everywhere that the web ACL and
-- its components are stored and used. Your changes are applied within
-- seconds, but there might be a brief period of inconsistency when the
-- changes have arrived in some places and not in others. So, for example,
-- if you change a rule action setting, the action might be the old action
-- in one area and the new action in another area. Or if you add an IP
-- address to an IP set used in a blocking rule, the new address might
-- briefly be blocked in one area while still allowed in another. This
-- temporary inconsistency can occur when you first associate a web ACL
-- with an Amazon Web Services resource and when you change a web ACL that
-- is already associated with a resource. Generally, any inconsistencies of
-- this type last only a few seconds.
module Amazonka.WAFV2.UpdateIPSet
  ( -- * Creating a Request
    UpdateIPSet (..),
    newUpdateIPSet,

    -- * Request Lenses
    updateIPSet_description,
    updateIPSet_name,
    updateIPSet_scope,
    updateIPSet_id,
    updateIPSet_addresses,
    updateIPSet_lockToken,

    -- * Destructuring the Response
    UpdateIPSetResponse (..),
    newUpdateIPSetResponse,

    -- * Response Lenses
    updateIPSetResponse_nextLockToken,
    updateIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { -- | A description of the IP set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the IP set. You cannot change the name of an @IPSet@ after
    -- you create it.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | A unique identifier for the set. This ID is returned in the responses to
    -- create and list commands. You provide it to operations like update and
    -- delete.
    id :: Prelude.Text,
    -- | Contains an array of strings that specifies zero or more IP addresses or
    -- blocks of IP addresses. All addresses must be specified using Classless
    -- Inter-Domain Routing (CIDR) notation. WAF supports all IPv4 and IPv6
    -- CIDR ranges except for @\/0@.
    --
    -- Example address strings:
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from IP addresses from 192.0.2.0 to 192.0.2.255, specify
    --     @192.0.2.0\/24@.
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify
    --     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to
    --     1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
    --     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
    --
    -- For more information about CIDR notation, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
    --
    -- Example JSON @Addresses@ specifications:
    --
    -- -   Empty array: @\"Addresses\": []@
    --
    -- -   Array with one address: @\"Addresses\": [\"192.0.2.44\/32\"]@
    --
    -- -   Array with three addresses:
    --     @\"Addresses\": [\"192.0.2.44\/32\", \"192.0.2.0\/24\", \"192.0.0.0\/16\"]@
    --
    -- -   INVALID specification: @\"Addresses\": [\"\"]@ INVALID
    addresses :: [Prelude.Text],
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateIPSet_description' - A description of the IP set that helps with identification.
--
-- 'name', 'updateIPSet_name' - The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
--
-- 'scope', 'updateIPSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'id', 'updateIPSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'addresses', 'updateIPSet_addresses' - Contains an array of strings that specifies zero or more IP addresses or
-- blocks of IP addresses. All addresses must be specified using Classless
-- Inter-Domain Routing (CIDR) notation. WAF supports all IPv4 and IPv6
-- CIDR ranges except for @\/0@.
--
-- Example address strings:
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses from 192.0.2.0 to 192.0.2.255, specify
--     @192.0.2.0\/24@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify
--     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to
--     1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
--     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- Example JSON @Addresses@ specifications:
--
-- -   Empty array: @\"Addresses\": []@
--
-- -   Array with one address: @\"Addresses\": [\"192.0.2.44\/32\"]@
--
-- -   Array with three addresses:
--     @\"Addresses\": [\"192.0.2.44\/32\", \"192.0.2.0\/24\", \"192.0.0.0\/16\"]@
--
-- -   INVALID specification: @\"Addresses\": [\"\"]@ INVALID
--
-- 'lockToken', 'updateIPSet_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newUpdateIPSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  UpdateIPSet
newUpdateIPSet pName_ pScope_ pId_ pLockToken_ =
  UpdateIPSet'
    { description = Prelude.Nothing,
      name = pName_,
      scope = pScope_,
      id = pId_,
      addresses = Prelude.mempty,
      lockToken = pLockToken_
    }

-- | A description of the IP set that helps with identification.
updateIPSet_description :: Lens.Lens' UpdateIPSet (Prelude.Maybe Prelude.Text)
updateIPSet_description = Lens.lens (\UpdateIPSet' {description} -> description) (\s@UpdateIPSet' {} a -> s {description = a} :: UpdateIPSet)

-- | The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
updateIPSet_name :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_name = Lens.lens (\UpdateIPSet' {name} -> name) (\s@UpdateIPSet' {} a -> s {name = a} :: UpdateIPSet)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
updateIPSet_scope :: Lens.Lens' UpdateIPSet Scope
updateIPSet_scope = Lens.lens (\UpdateIPSet' {scope} -> scope) (\s@UpdateIPSet' {} a -> s {scope = a} :: UpdateIPSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
updateIPSet_id :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_id = Lens.lens (\UpdateIPSet' {id} -> id) (\s@UpdateIPSet' {} a -> s {id = a} :: UpdateIPSet)

-- | Contains an array of strings that specifies zero or more IP addresses or
-- blocks of IP addresses. All addresses must be specified using Classless
-- Inter-Domain Routing (CIDR) notation. WAF supports all IPv4 and IPv6
-- CIDR ranges except for @\/0@.
--
-- Example address strings:
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses from 192.0.2.0 to 192.0.2.255, specify
--     @192.0.2.0\/24@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify
--     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to
--     1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
--     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- Example JSON @Addresses@ specifications:
--
-- -   Empty array: @\"Addresses\": []@
--
-- -   Array with one address: @\"Addresses\": [\"192.0.2.44\/32\"]@
--
-- -   Array with three addresses:
--     @\"Addresses\": [\"192.0.2.44\/32\", \"192.0.2.0\/24\", \"192.0.0.0\/16\"]@
--
-- -   INVALID specification: @\"Addresses\": [\"\"]@ INVALID
updateIPSet_addresses :: Lens.Lens' UpdateIPSet [Prelude.Text]
updateIPSet_addresses = Lens.lens (\UpdateIPSet' {addresses} -> addresses) (\s@UpdateIPSet' {} a -> s {addresses = a} :: UpdateIPSet) Prelude.. Lens.coerced

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
updateIPSet_lockToken :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_lockToken = Lens.lens (\UpdateIPSet' {lockToken} -> lockToken) (\s@UpdateIPSet' {} a -> s {lockToken = a} :: UpdateIPSet)

instance Core.AWSRequest UpdateIPSet where
  type AWSResponse UpdateIPSet = UpdateIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIPSetResponse'
            Prelude.<$> (x Data..?> "NextLockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIPSet where
  hashWithSalt _salt UpdateIPSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` addresses
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData UpdateIPSet where
  rnf UpdateIPSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders UpdateIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.UpdateIPSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIPSet where
  toJSON UpdateIPSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Addresses" Data..= addresses),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath UpdateIPSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
  { -- | A token used for optimistic locking. WAF returns this token to your
    -- @update@ requests. You use @NextLockToken@ in the same manner as you use
    -- @LockToken@.
    nextLockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextLockToken', 'updateIPSetResponse_nextLockToken' - A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
--
-- 'httpStatus', 'updateIPSetResponse_httpStatus' - The response's http status code.
newUpdateIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIPSetResponse
newUpdateIPSetResponse pHttpStatus_ =
  UpdateIPSetResponse'
    { nextLockToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
updateIPSetResponse_nextLockToken :: Lens.Lens' UpdateIPSetResponse (Prelude.Maybe Prelude.Text)
updateIPSetResponse_nextLockToken = Lens.lens (\UpdateIPSetResponse' {nextLockToken} -> nextLockToken) (\s@UpdateIPSetResponse' {} a -> s {nextLockToken = a} :: UpdateIPSetResponse)

-- | The response's http status code.
updateIPSetResponse_httpStatus :: Lens.Lens' UpdateIPSetResponse Prelude.Int
updateIPSetResponse_httpStatus = Lens.lens (\UpdateIPSetResponse' {httpStatus} -> httpStatus) (\s@UpdateIPSetResponse' {} a -> s {httpStatus = a} :: UpdateIPSetResponse)

instance Prelude.NFData UpdateIPSetResponse where
  rnf UpdateIPSetResponse' {..} =
    Prelude.rnf nextLockToken
      `Prelude.seq` Prelude.rnf httpStatus
