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
-- Module      : Amazonka.WAFV2.CreateIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IPSet, which you use to identify web requests that originate
-- from specific IP addresses or ranges of IP addresses. For example, if
-- you\'re receiving a lot of requests from a ranges of IP addresses, you
-- can configure WAF to block them using an IPSet that lists those IP
-- addresses.
module Amazonka.WAFV2.CreateIPSet
  ( -- * Creating a Request
    CreateIPSet (..),
    newCreateIPSet,

    -- * Request Lenses
    createIPSet_description,
    createIPSet_tags,
    createIPSet_name,
    createIPSet_scope,
    createIPSet_iPAddressVersion,
    createIPSet_addresses,

    -- * Destructuring the Response
    CreateIPSetResponse (..),
    newCreateIPSetResponse,

    -- * Response Lenses
    createIPSetResponse_summary,
    createIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { -- | A description of the IP set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | An array of key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
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
    -- | The version of the IP addresses, either @IPV4@ or @IPV6@.
    iPAddressVersion :: IPAddressVersion,
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
    addresses :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createIPSet_description' - A description of the IP set that helps with identification.
--
-- 'tags', 'createIPSet_tags' - An array of key:value pairs to associate with the resource.
--
-- 'name', 'createIPSet_name' - The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
--
-- 'scope', 'createIPSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'iPAddressVersion', 'createIPSet_iPAddressVersion' - The version of the IP addresses, either @IPV4@ or @IPV6@.
--
-- 'addresses', 'createIPSet_addresses' - Contains an array of strings that specifies zero or more IP addresses or
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
newCreateIPSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'iPAddressVersion'
  IPAddressVersion ->
  CreateIPSet
newCreateIPSet pName_ pScope_ pIPAddressVersion_ =
  CreateIPSet'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      scope = pScope_,
      iPAddressVersion = pIPAddressVersion_,
      addresses = Prelude.mempty
    }

-- | A description of the IP set that helps with identification.
createIPSet_description :: Lens.Lens' CreateIPSet (Prelude.Maybe Prelude.Text)
createIPSet_description = Lens.lens (\CreateIPSet' {description} -> description) (\s@CreateIPSet' {} a -> s {description = a} :: CreateIPSet)

-- | An array of key:value pairs to associate with the resource.
createIPSet_tags :: Lens.Lens' CreateIPSet (Prelude.Maybe (Prelude.NonEmpty Tag))
createIPSet_tags = Lens.lens (\CreateIPSet' {tags} -> tags) (\s@CreateIPSet' {} a -> s {tags = a} :: CreateIPSet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
createIPSet_name :: Lens.Lens' CreateIPSet Prelude.Text
createIPSet_name = Lens.lens (\CreateIPSet' {name} -> name) (\s@CreateIPSet' {} a -> s {name = a} :: CreateIPSet)

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
createIPSet_scope :: Lens.Lens' CreateIPSet Scope
createIPSet_scope = Lens.lens (\CreateIPSet' {scope} -> scope) (\s@CreateIPSet' {} a -> s {scope = a} :: CreateIPSet)

-- | The version of the IP addresses, either @IPV4@ or @IPV6@.
createIPSet_iPAddressVersion :: Lens.Lens' CreateIPSet IPAddressVersion
createIPSet_iPAddressVersion = Lens.lens (\CreateIPSet' {iPAddressVersion} -> iPAddressVersion) (\s@CreateIPSet' {} a -> s {iPAddressVersion = a} :: CreateIPSet)

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
createIPSet_addresses :: Lens.Lens' CreateIPSet [Prelude.Text]
createIPSet_addresses = Lens.lens (\CreateIPSet' {addresses} -> addresses) (\s@CreateIPSet' {} a -> s {addresses = a} :: CreateIPSet) Prelude.. Lens.coerced

instance Core.AWSRequest CreateIPSet where
  type AWSResponse CreateIPSet = CreateIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Prelude.<$> (x Data..?> "Summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIPSet where
  hashWithSalt _salt CreateIPSet' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` iPAddressVersion
      `Prelude.hashWithSalt` addresses

instance Prelude.NFData CreateIPSet where
  rnf CreateIPSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf iPAddressVersion
      `Prelude.seq` Prelude.rnf addresses

instance Data.ToHeaders CreateIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.CreateIPSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIPSet where
  toJSON CreateIPSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just
              ("IPAddressVersion" Data..= iPAddressVersion),
            Prelude.Just ("Addresses" Data..= addresses)
          ]
      )

instance Data.ToPath CreateIPSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { -- | High-level information about an IPSet, returned by operations like
    -- create and list. This provides information like the ID, that you can use
    -- to retrieve and manage an @IPSet@, and the ARN, that you provide to the
    -- IPSetReferenceStatement to use the address set in a Rule.
    summary :: Prelude.Maybe IPSetSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'createIPSetResponse_summary' - High-level information about an IPSet, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage an @IPSet@, and the ARN, that you provide to the
-- IPSetReferenceStatement to use the address set in a Rule.
--
-- 'httpStatus', 'createIPSetResponse_httpStatus' - The response's http status code.
newCreateIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIPSetResponse
newCreateIPSetResponse pHttpStatus_ =
  CreateIPSetResponse'
    { summary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | High-level information about an IPSet, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage an @IPSet@, and the ARN, that you provide to the
-- IPSetReferenceStatement to use the address set in a Rule.
createIPSetResponse_summary :: Lens.Lens' CreateIPSetResponse (Prelude.Maybe IPSetSummary)
createIPSetResponse_summary = Lens.lens (\CreateIPSetResponse' {summary} -> summary) (\s@CreateIPSetResponse' {} a -> s {summary = a} :: CreateIPSetResponse)

-- | The response's http status code.
createIPSetResponse_httpStatus :: Lens.Lens' CreateIPSetResponse Prelude.Int
createIPSetResponse_httpStatus = Lens.lens (\CreateIPSetResponse' {httpStatus} -> httpStatus) (\s@CreateIPSetResponse' {} a -> s {httpStatus = a} :: CreateIPSetResponse)

instance Prelude.NFData CreateIPSetResponse where
  rnf CreateIPSetResponse' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf httpStatus
