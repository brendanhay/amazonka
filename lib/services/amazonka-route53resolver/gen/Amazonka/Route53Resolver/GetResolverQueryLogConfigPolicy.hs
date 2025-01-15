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
-- Module      : Amazonka.Route53Resolver.GetResolverQueryLogConfigPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a query logging policy. A query logging policy
-- specifies the Resolver query logging operations and resources that you
-- want to allow another Amazon Web Services account to be able to use.
module Amazonka.Route53Resolver.GetResolverQueryLogConfigPolicy
  ( -- * Creating a Request
    GetResolverQueryLogConfigPolicy (..),
    newGetResolverQueryLogConfigPolicy,

    -- * Request Lenses
    getResolverQueryLogConfigPolicy_arn,

    -- * Destructuring the Response
    GetResolverQueryLogConfigPolicyResponse (..),
    newGetResolverQueryLogConfigPolicyResponse,

    -- * Response Lenses
    getResolverQueryLogConfigPolicyResponse_resolverQueryLogConfigPolicy,
    getResolverQueryLogConfigPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverQueryLogConfigPolicy' smart constructor.
data GetResolverQueryLogConfigPolicy = GetResolverQueryLogConfigPolicy'
  { -- | The ARN of the query logging configuration that you want to get the
    -- query logging policy for.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverQueryLogConfigPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getResolverQueryLogConfigPolicy_arn' - The ARN of the query logging configuration that you want to get the
-- query logging policy for.
newGetResolverQueryLogConfigPolicy ::
  -- | 'arn'
  Prelude.Text ->
  GetResolverQueryLogConfigPolicy
newGetResolverQueryLogConfigPolicy pArn_ =
  GetResolverQueryLogConfigPolicy' {arn = pArn_}

-- | The ARN of the query logging configuration that you want to get the
-- query logging policy for.
getResolverQueryLogConfigPolicy_arn :: Lens.Lens' GetResolverQueryLogConfigPolicy Prelude.Text
getResolverQueryLogConfigPolicy_arn = Lens.lens (\GetResolverQueryLogConfigPolicy' {arn} -> arn) (\s@GetResolverQueryLogConfigPolicy' {} a -> s {arn = a} :: GetResolverQueryLogConfigPolicy)

instance
  Core.AWSRequest
    GetResolverQueryLogConfigPolicy
  where
  type
    AWSResponse GetResolverQueryLogConfigPolicy =
      GetResolverQueryLogConfigPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverQueryLogConfigPolicyResponse'
            Prelude.<$> (x Data..?> "ResolverQueryLogConfigPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResolverQueryLogConfigPolicy
  where
  hashWithSalt
    _salt
    GetResolverQueryLogConfigPolicy' {..} =
      _salt `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    GetResolverQueryLogConfigPolicy
  where
  rnf GetResolverQueryLogConfigPolicy' {..} =
    Prelude.rnf arn

instance
  Data.ToHeaders
    GetResolverQueryLogConfigPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetResolverQueryLogConfigPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResolverQueryLogConfigPolicy where
  toJSON GetResolverQueryLogConfigPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath GetResolverQueryLogConfigPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResolverQueryLogConfigPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverQueryLogConfigPolicyResponse' smart constructor.
data GetResolverQueryLogConfigPolicyResponse = GetResolverQueryLogConfigPolicyResponse'
  { -- | Information about the query logging policy for the query logging
    -- configuration that you specified in a @GetResolverQueryLogConfigPolicy@
    -- request.
    resolverQueryLogConfigPolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverQueryLogConfigPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigPolicy', 'getResolverQueryLogConfigPolicyResponse_resolverQueryLogConfigPolicy' - Information about the query logging policy for the query logging
-- configuration that you specified in a @GetResolverQueryLogConfigPolicy@
-- request.
--
-- 'httpStatus', 'getResolverQueryLogConfigPolicyResponse_httpStatus' - The response's http status code.
newGetResolverQueryLogConfigPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverQueryLogConfigPolicyResponse
newGetResolverQueryLogConfigPolicyResponse
  pHttpStatus_ =
    GetResolverQueryLogConfigPolicyResponse'
      { resolverQueryLogConfigPolicy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the query logging policy for the query logging
-- configuration that you specified in a @GetResolverQueryLogConfigPolicy@
-- request.
getResolverQueryLogConfigPolicyResponse_resolverQueryLogConfigPolicy :: Lens.Lens' GetResolverQueryLogConfigPolicyResponse (Prelude.Maybe Prelude.Text)
getResolverQueryLogConfigPolicyResponse_resolverQueryLogConfigPolicy = Lens.lens (\GetResolverQueryLogConfigPolicyResponse' {resolverQueryLogConfigPolicy} -> resolverQueryLogConfigPolicy) (\s@GetResolverQueryLogConfigPolicyResponse' {} a -> s {resolverQueryLogConfigPolicy = a} :: GetResolverQueryLogConfigPolicyResponse)

-- | The response's http status code.
getResolverQueryLogConfigPolicyResponse_httpStatus :: Lens.Lens' GetResolverQueryLogConfigPolicyResponse Prelude.Int
getResolverQueryLogConfigPolicyResponse_httpStatus = Lens.lens (\GetResolverQueryLogConfigPolicyResponse' {httpStatus} -> httpStatus) (\s@GetResolverQueryLogConfigPolicyResponse' {} a -> s {httpStatus = a} :: GetResolverQueryLogConfigPolicyResponse)

instance
  Prelude.NFData
    GetResolverQueryLogConfigPolicyResponse
  where
  rnf GetResolverQueryLogConfigPolicyResponse' {..} =
    Prelude.rnf resolverQueryLogConfigPolicy `Prelude.seq`
      Prelude.rnf httpStatus
