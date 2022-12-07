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
-- Module      : Amazonka.Route53Resolver.GetResolverRuleAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an association between a specified Resolver rule
-- and a VPC. You associate a Resolver rule and a VPC using
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverRule.html AssociateResolverRule>.
module Amazonka.Route53Resolver.GetResolverRuleAssociation
  ( -- * Creating a Request
    GetResolverRuleAssociation (..),
    newGetResolverRuleAssociation,

    -- * Request Lenses
    getResolverRuleAssociation_resolverRuleAssociationId,

    -- * Destructuring the Response
    GetResolverRuleAssociationResponse (..),
    newGetResolverRuleAssociationResponse,

    -- * Response Lenses
    getResolverRuleAssociationResponse_resolverRuleAssociation,
    getResolverRuleAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverRuleAssociation' smart constructor.
data GetResolverRuleAssociation = GetResolverRuleAssociation'
  { -- | The ID of the Resolver rule association that you want to get information
    -- about.
    resolverRuleAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverRuleAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRuleAssociationId', 'getResolverRuleAssociation_resolverRuleAssociationId' - The ID of the Resolver rule association that you want to get information
-- about.
newGetResolverRuleAssociation ::
  -- | 'resolverRuleAssociationId'
  Prelude.Text ->
  GetResolverRuleAssociation
newGetResolverRuleAssociation
  pResolverRuleAssociationId_ =
    GetResolverRuleAssociation'
      { resolverRuleAssociationId =
          pResolverRuleAssociationId_
      }

-- | The ID of the Resolver rule association that you want to get information
-- about.
getResolverRuleAssociation_resolverRuleAssociationId :: Lens.Lens' GetResolverRuleAssociation Prelude.Text
getResolverRuleAssociation_resolverRuleAssociationId = Lens.lens (\GetResolverRuleAssociation' {resolverRuleAssociationId} -> resolverRuleAssociationId) (\s@GetResolverRuleAssociation' {} a -> s {resolverRuleAssociationId = a} :: GetResolverRuleAssociation)

instance Core.AWSRequest GetResolverRuleAssociation where
  type
    AWSResponse GetResolverRuleAssociation =
      GetResolverRuleAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverRuleAssociationResponse'
            Prelude.<$> (x Data..?> "ResolverRuleAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverRuleAssociation where
  hashWithSalt _salt GetResolverRuleAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` resolverRuleAssociationId

instance Prelude.NFData GetResolverRuleAssociation where
  rnf GetResolverRuleAssociation' {..} =
    Prelude.rnf resolverRuleAssociationId

instance Data.ToHeaders GetResolverRuleAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetResolverRuleAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResolverRuleAssociation where
  toJSON GetResolverRuleAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResolverRuleAssociationId"
                  Data..= resolverRuleAssociationId
              )
          ]
      )

instance Data.ToPath GetResolverRuleAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResolverRuleAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverRuleAssociationResponse' smart constructor.
data GetResolverRuleAssociationResponse = GetResolverRuleAssociationResponse'
  { -- | Information about the Resolver rule association that you specified in a
    -- @GetResolverRuleAssociation@ request.
    resolverRuleAssociation :: Prelude.Maybe ResolverRuleAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverRuleAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRuleAssociation', 'getResolverRuleAssociationResponse_resolverRuleAssociation' - Information about the Resolver rule association that you specified in a
-- @GetResolverRuleAssociation@ request.
--
-- 'httpStatus', 'getResolverRuleAssociationResponse_httpStatus' - The response's http status code.
newGetResolverRuleAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverRuleAssociationResponse
newGetResolverRuleAssociationResponse pHttpStatus_ =
  GetResolverRuleAssociationResponse'
    { resolverRuleAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Resolver rule association that you specified in a
-- @GetResolverRuleAssociation@ request.
getResolverRuleAssociationResponse_resolverRuleAssociation :: Lens.Lens' GetResolverRuleAssociationResponse (Prelude.Maybe ResolverRuleAssociation)
getResolverRuleAssociationResponse_resolverRuleAssociation = Lens.lens (\GetResolverRuleAssociationResponse' {resolverRuleAssociation} -> resolverRuleAssociation) (\s@GetResolverRuleAssociationResponse' {} a -> s {resolverRuleAssociation = a} :: GetResolverRuleAssociationResponse)

-- | The response's http status code.
getResolverRuleAssociationResponse_httpStatus :: Lens.Lens' GetResolverRuleAssociationResponse Prelude.Int
getResolverRuleAssociationResponse_httpStatus = Lens.lens (\GetResolverRuleAssociationResponse' {httpStatus} -> httpStatus) (\s@GetResolverRuleAssociationResponse' {} a -> s {httpStatus = a} :: GetResolverRuleAssociationResponse)

instance
  Prelude.NFData
    GetResolverRuleAssociationResponse
  where
  rnf GetResolverRuleAssociationResponse' {..} =
    Prelude.rnf resolverRuleAssociation
      `Prelude.seq` Prelude.rnf httpStatus
