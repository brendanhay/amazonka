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
-- Module      : Amazonka.Route53Resolver.PutResolverQueryLogConfigPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies an Amazon Web Services account that you want to share a query
-- logging configuration with, the query logging configuration that you
-- want to share, and the operations that you want the account to be able
-- to perform on the configuration.
module Amazonka.Route53Resolver.PutResolverQueryLogConfigPolicy
  ( -- * Creating a Request
    PutResolverQueryLogConfigPolicy (..),
    newPutResolverQueryLogConfigPolicy,

    -- * Request Lenses
    putResolverQueryLogConfigPolicy_arn,
    putResolverQueryLogConfigPolicy_resolverQueryLogConfigPolicy,

    -- * Destructuring the Response
    PutResolverQueryLogConfigPolicyResponse (..),
    newPutResolverQueryLogConfigPolicyResponse,

    -- * Response Lenses
    putResolverQueryLogConfigPolicyResponse_returnValue,
    putResolverQueryLogConfigPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newPutResolverQueryLogConfigPolicy' smart constructor.
data PutResolverQueryLogConfigPolicy = PutResolverQueryLogConfigPolicy'
  { -- | The Amazon Resource Name (ARN) of the account that you want to share
    -- rules with.
    arn :: Prelude.Text,
    -- | An Identity and Access Management policy statement that lists the query
    -- logging configurations that you want to share with another Amazon Web
    -- Services account and the operations that you want the account to be able
    -- to perform. You can specify the following operations in the @Actions@
    -- section of the statement:
    --
    -- -   @route53resolver:AssociateResolverQueryLogConfig@
    --
    -- -   @route53resolver:DisassociateResolverQueryLogConfig@
    --
    -- -   @route53resolver:ListResolverQueryLogConfigAssociations@
    --
    -- -   @route53resolver:ListResolverQueryLogConfigs@
    --
    -- In the @Resource@ section of the statement, you specify the ARNs for the
    -- query logging configurations that you want to share with the account
    -- that you specified in @Arn@.
    resolverQueryLogConfigPolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResolverQueryLogConfigPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'putResolverQueryLogConfigPolicy_arn' - The Amazon Resource Name (ARN) of the account that you want to share
-- rules with.
--
-- 'resolverQueryLogConfigPolicy', 'putResolverQueryLogConfigPolicy_resolverQueryLogConfigPolicy' - An Identity and Access Management policy statement that lists the query
-- logging configurations that you want to share with another Amazon Web
-- Services account and the operations that you want the account to be able
-- to perform. You can specify the following operations in the @Actions@
-- section of the statement:
--
-- -   @route53resolver:AssociateResolverQueryLogConfig@
--
-- -   @route53resolver:DisassociateResolverQueryLogConfig@
--
-- -   @route53resolver:ListResolverQueryLogConfigAssociations@
--
-- -   @route53resolver:ListResolverQueryLogConfigs@
--
-- In the @Resource@ section of the statement, you specify the ARNs for the
-- query logging configurations that you want to share with the account
-- that you specified in @Arn@.
newPutResolverQueryLogConfigPolicy ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'resolverQueryLogConfigPolicy'
  Prelude.Text ->
  PutResolverQueryLogConfigPolicy
newPutResolverQueryLogConfigPolicy
  pArn_
  pResolverQueryLogConfigPolicy_ =
    PutResolverQueryLogConfigPolicy'
      { arn = pArn_,
        resolverQueryLogConfigPolicy =
          pResolverQueryLogConfigPolicy_
      }

-- | The Amazon Resource Name (ARN) of the account that you want to share
-- rules with.
putResolverQueryLogConfigPolicy_arn :: Lens.Lens' PutResolverQueryLogConfigPolicy Prelude.Text
putResolverQueryLogConfigPolicy_arn = Lens.lens (\PutResolverQueryLogConfigPolicy' {arn} -> arn) (\s@PutResolverQueryLogConfigPolicy' {} a -> s {arn = a} :: PutResolverQueryLogConfigPolicy)

-- | An Identity and Access Management policy statement that lists the query
-- logging configurations that you want to share with another Amazon Web
-- Services account and the operations that you want the account to be able
-- to perform. You can specify the following operations in the @Actions@
-- section of the statement:
--
-- -   @route53resolver:AssociateResolverQueryLogConfig@
--
-- -   @route53resolver:DisassociateResolverQueryLogConfig@
--
-- -   @route53resolver:ListResolverQueryLogConfigAssociations@
--
-- -   @route53resolver:ListResolverQueryLogConfigs@
--
-- In the @Resource@ section of the statement, you specify the ARNs for the
-- query logging configurations that you want to share with the account
-- that you specified in @Arn@.
putResolverQueryLogConfigPolicy_resolverQueryLogConfigPolicy :: Lens.Lens' PutResolverQueryLogConfigPolicy Prelude.Text
putResolverQueryLogConfigPolicy_resolverQueryLogConfigPolicy = Lens.lens (\PutResolverQueryLogConfigPolicy' {resolverQueryLogConfigPolicy} -> resolverQueryLogConfigPolicy) (\s@PutResolverQueryLogConfigPolicy' {} a -> s {resolverQueryLogConfigPolicy = a} :: PutResolverQueryLogConfigPolicy)

instance
  Core.AWSRequest
    PutResolverQueryLogConfigPolicy
  where
  type
    AWSResponse PutResolverQueryLogConfigPolicy =
      PutResolverQueryLogConfigPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResolverQueryLogConfigPolicyResponse'
            Prelude.<$> (x Core..?> "ReturnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutResolverQueryLogConfigPolicy
  where
  hashWithSalt
    _salt
    PutResolverQueryLogConfigPolicy' {..} =
      _salt `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` resolverQueryLogConfigPolicy

instance
  Prelude.NFData
    PutResolverQueryLogConfigPolicy
  where
  rnf PutResolverQueryLogConfigPolicy' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resolverQueryLogConfigPolicy

instance
  Core.ToHeaders
    PutResolverQueryLogConfigPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.PutResolverQueryLogConfigPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutResolverQueryLogConfigPolicy where
  toJSON PutResolverQueryLogConfigPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Core..= arn),
            Prelude.Just
              ( "ResolverQueryLogConfigPolicy"
                  Core..= resolverQueryLogConfigPolicy
              )
          ]
      )

instance Core.ToPath PutResolverQueryLogConfigPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutResolverQueryLogConfigPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a @PutResolverQueryLogConfigPolicy@ request.
--
-- /See:/ 'newPutResolverQueryLogConfigPolicyResponse' smart constructor.
data PutResolverQueryLogConfigPolicyResponse = PutResolverQueryLogConfigPolicyResponse'
  { -- | Whether the @PutResolverQueryLogConfigPolicy@ request was successful.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResolverQueryLogConfigPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'putResolverQueryLogConfigPolicyResponse_returnValue' - Whether the @PutResolverQueryLogConfigPolicy@ request was successful.
--
-- 'httpStatus', 'putResolverQueryLogConfigPolicyResponse_httpStatus' - The response's http status code.
newPutResolverQueryLogConfigPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResolverQueryLogConfigPolicyResponse
newPutResolverQueryLogConfigPolicyResponse
  pHttpStatus_ =
    PutResolverQueryLogConfigPolicyResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Whether the @PutResolverQueryLogConfigPolicy@ request was successful.
putResolverQueryLogConfigPolicyResponse_returnValue :: Lens.Lens' PutResolverQueryLogConfigPolicyResponse (Prelude.Maybe Prelude.Bool)
putResolverQueryLogConfigPolicyResponse_returnValue = Lens.lens (\PutResolverQueryLogConfigPolicyResponse' {returnValue} -> returnValue) (\s@PutResolverQueryLogConfigPolicyResponse' {} a -> s {returnValue = a} :: PutResolverQueryLogConfigPolicyResponse)

-- | The response's http status code.
putResolverQueryLogConfigPolicyResponse_httpStatus :: Lens.Lens' PutResolverQueryLogConfigPolicyResponse Prelude.Int
putResolverQueryLogConfigPolicyResponse_httpStatus = Lens.lens (\PutResolverQueryLogConfigPolicyResponse' {httpStatus} -> httpStatus) (\s@PutResolverQueryLogConfigPolicyResponse' {} a -> s {httpStatus = a} :: PutResolverQueryLogConfigPolicyResponse)

instance
  Prelude.NFData
    PutResolverQueryLogConfigPolicyResponse
  where
  rnf PutResolverQueryLogConfigPolicyResponse' {..} =
    Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
