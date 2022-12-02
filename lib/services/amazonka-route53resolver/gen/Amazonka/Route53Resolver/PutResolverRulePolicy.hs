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
-- Module      : Amazonka.Route53Resolver.PutResolverRulePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies an Amazon Web Services rule that you want to share with
-- another account, the account that you want to share the rule with, and
-- the operations that you want the account to be able to perform on the
-- rule.
module Amazonka.Route53Resolver.PutResolverRulePolicy
  ( -- * Creating a Request
    PutResolverRulePolicy (..),
    newPutResolverRulePolicy,

    -- * Request Lenses
    putResolverRulePolicy_arn,
    putResolverRulePolicy_resolverRulePolicy,

    -- * Destructuring the Response
    PutResolverRulePolicyResponse (..),
    newPutResolverRulePolicyResponse,

    -- * Response Lenses
    putResolverRulePolicyResponse_returnValue,
    putResolverRulePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newPutResolverRulePolicy' smart constructor.
data PutResolverRulePolicy = PutResolverRulePolicy'
  { -- | The Amazon Resource Name (ARN) of the rule that you want to share with
    -- another account.
    arn :: Prelude.Text,
    -- | An Identity and Access Management policy statement that lists the rules
    -- that you want to share with another Amazon Web Services account and the
    -- operations that you want the account to be able to perform. You can
    -- specify the following operations in the @Action@ section of the
    -- statement:
    --
    -- -   @route53resolver:GetResolverRule@
    --
    -- -   @route53resolver:AssociateResolverRule@
    --
    -- -   @route53resolver:DisassociateResolverRule@
    --
    -- -   @route53resolver:ListResolverRules@
    --
    -- -   @route53resolver:ListResolverRuleAssociations@
    --
    -- In the @Resource@ section of the statement, specify the ARN for the rule
    -- that you want to share with another account. Specify the same ARN that
    -- you specified in @Arn@.
    resolverRulePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResolverRulePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'putResolverRulePolicy_arn' - The Amazon Resource Name (ARN) of the rule that you want to share with
-- another account.
--
-- 'resolverRulePolicy', 'putResolverRulePolicy_resolverRulePolicy' - An Identity and Access Management policy statement that lists the rules
-- that you want to share with another Amazon Web Services account and the
-- operations that you want the account to be able to perform. You can
-- specify the following operations in the @Action@ section of the
-- statement:
--
-- -   @route53resolver:GetResolverRule@
--
-- -   @route53resolver:AssociateResolverRule@
--
-- -   @route53resolver:DisassociateResolverRule@
--
-- -   @route53resolver:ListResolverRules@
--
-- -   @route53resolver:ListResolverRuleAssociations@
--
-- In the @Resource@ section of the statement, specify the ARN for the rule
-- that you want to share with another account. Specify the same ARN that
-- you specified in @Arn@.
newPutResolverRulePolicy ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'resolverRulePolicy'
  Prelude.Text ->
  PutResolverRulePolicy
newPutResolverRulePolicy pArn_ pResolverRulePolicy_ =
  PutResolverRulePolicy'
    { arn = pArn_,
      resolverRulePolicy = pResolverRulePolicy_
    }

-- | The Amazon Resource Name (ARN) of the rule that you want to share with
-- another account.
putResolverRulePolicy_arn :: Lens.Lens' PutResolverRulePolicy Prelude.Text
putResolverRulePolicy_arn = Lens.lens (\PutResolverRulePolicy' {arn} -> arn) (\s@PutResolverRulePolicy' {} a -> s {arn = a} :: PutResolverRulePolicy)

-- | An Identity and Access Management policy statement that lists the rules
-- that you want to share with another Amazon Web Services account and the
-- operations that you want the account to be able to perform. You can
-- specify the following operations in the @Action@ section of the
-- statement:
--
-- -   @route53resolver:GetResolverRule@
--
-- -   @route53resolver:AssociateResolverRule@
--
-- -   @route53resolver:DisassociateResolverRule@
--
-- -   @route53resolver:ListResolverRules@
--
-- -   @route53resolver:ListResolverRuleAssociations@
--
-- In the @Resource@ section of the statement, specify the ARN for the rule
-- that you want to share with another account. Specify the same ARN that
-- you specified in @Arn@.
putResolverRulePolicy_resolverRulePolicy :: Lens.Lens' PutResolverRulePolicy Prelude.Text
putResolverRulePolicy_resolverRulePolicy = Lens.lens (\PutResolverRulePolicy' {resolverRulePolicy} -> resolverRulePolicy) (\s@PutResolverRulePolicy' {} a -> s {resolverRulePolicy = a} :: PutResolverRulePolicy)

instance Core.AWSRequest PutResolverRulePolicy where
  type
    AWSResponse PutResolverRulePolicy =
      PutResolverRulePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResolverRulePolicyResponse'
            Prelude.<$> (x Data..?> "ReturnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResolverRulePolicy where
  hashWithSalt _salt PutResolverRulePolicy' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resolverRulePolicy

instance Prelude.NFData PutResolverRulePolicy where
  rnf PutResolverRulePolicy' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resolverRulePolicy

instance Data.ToHeaders PutResolverRulePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.PutResolverRulePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResolverRulePolicy where
  toJSON PutResolverRulePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("ResolverRulePolicy" Data..= resolverRulePolicy)
          ]
      )

instance Data.ToPath PutResolverRulePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResolverRulePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a @PutResolverRulePolicy@ request.
--
-- /See:/ 'newPutResolverRulePolicyResponse' smart constructor.
data PutResolverRulePolicyResponse = PutResolverRulePolicyResponse'
  { -- | Whether the @PutResolverRulePolicy@ request was successful.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResolverRulePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'putResolverRulePolicyResponse_returnValue' - Whether the @PutResolverRulePolicy@ request was successful.
--
-- 'httpStatus', 'putResolverRulePolicyResponse_httpStatus' - The response's http status code.
newPutResolverRulePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResolverRulePolicyResponse
newPutResolverRulePolicyResponse pHttpStatus_ =
  PutResolverRulePolicyResponse'
    { returnValue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Whether the @PutResolverRulePolicy@ request was successful.
putResolverRulePolicyResponse_returnValue :: Lens.Lens' PutResolverRulePolicyResponse (Prelude.Maybe Prelude.Bool)
putResolverRulePolicyResponse_returnValue = Lens.lens (\PutResolverRulePolicyResponse' {returnValue} -> returnValue) (\s@PutResolverRulePolicyResponse' {} a -> s {returnValue = a} :: PutResolverRulePolicyResponse)

-- | The response's http status code.
putResolverRulePolicyResponse_httpStatus :: Lens.Lens' PutResolverRulePolicyResponse Prelude.Int
putResolverRulePolicyResponse_httpStatus = Lens.lens (\PutResolverRulePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResolverRulePolicyResponse' {} a -> s {httpStatus = a} :: PutResolverRulePolicyResponse)

instance Prelude.NFData PutResolverRulePolicyResponse where
  rnf PutResolverRulePolicyResponse' {..} =
    Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
