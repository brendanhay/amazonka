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
-- Module      : Amazonka.Route53Resolver.GetResolverQueryLogConfigAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified association between a Resolver query
-- logging configuration and an Amazon VPC. When you associate a VPC with a
-- query logging configuration, Resolver logs DNS queries that originate in
-- that VPC.
module Amazonka.Route53Resolver.GetResolverQueryLogConfigAssociation
  ( -- * Creating a Request
    GetResolverQueryLogConfigAssociation (..),
    newGetResolverQueryLogConfigAssociation,

    -- * Request Lenses
    getResolverQueryLogConfigAssociation_resolverQueryLogConfigAssociationId,

    -- * Destructuring the Response
    GetResolverQueryLogConfigAssociationResponse (..),
    newGetResolverQueryLogConfigAssociationResponse,

    -- * Response Lenses
    getResolverQueryLogConfigAssociationResponse_resolverQueryLogConfigAssociation,
    getResolverQueryLogConfigAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverQueryLogConfigAssociation' smart constructor.
data GetResolverQueryLogConfigAssociation = GetResolverQueryLogConfigAssociation'
  { -- | The ID of the Resolver query logging configuration association that you
    -- want to get information about.
    resolverQueryLogConfigAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverQueryLogConfigAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigAssociationId', 'getResolverQueryLogConfigAssociation_resolverQueryLogConfigAssociationId' - The ID of the Resolver query logging configuration association that you
-- want to get information about.
newGetResolverQueryLogConfigAssociation ::
  -- | 'resolverQueryLogConfigAssociationId'
  Prelude.Text ->
  GetResolverQueryLogConfigAssociation
newGetResolverQueryLogConfigAssociation
  pResolverQueryLogConfigAssociationId_ =
    GetResolverQueryLogConfigAssociation'
      { resolverQueryLogConfigAssociationId =
          pResolverQueryLogConfigAssociationId_
      }

-- | The ID of the Resolver query logging configuration association that you
-- want to get information about.
getResolverQueryLogConfigAssociation_resolverQueryLogConfigAssociationId :: Lens.Lens' GetResolverQueryLogConfigAssociation Prelude.Text
getResolverQueryLogConfigAssociation_resolverQueryLogConfigAssociationId = Lens.lens (\GetResolverQueryLogConfigAssociation' {resolverQueryLogConfigAssociationId} -> resolverQueryLogConfigAssociationId) (\s@GetResolverQueryLogConfigAssociation' {} a -> s {resolverQueryLogConfigAssociationId = a} :: GetResolverQueryLogConfigAssociation)

instance
  Core.AWSRequest
    GetResolverQueryLogConfigAssociation
  where
  type
    AWSResponse GetResolverQueryLogConfigAssociation =
      GetResolverQueryLogConfigAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverQueryLogConfigAssociationResponse'
            Prelude.<$> (x Data..?> "ResolverQueryLogConfigAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResolverQueryLogConfigAssociation
  where
  hashWithSalt
    _salt
    GetResolverQueryLogConfigAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` resolverQueryLogConfigAssociationId

instance
  Prelude.NFData
    GetResolverQueryLogConfigAssociation
  where
  rnf GetResolverQueryLogConfigAssociation' {..} =
    Prelude.rnf resolverQueryLogConfigAssociationId

instance
  Data.ToHeaders
    GetResolverQueryLogConfigAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetResolverQueryLogConfigAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetResolverQueryLogConfigAssociation
  where
  toJSON GetResolverQueryLogConfigAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResolverQueryLogConfigAssociationId"
                  Data..= resolverQueryLogConfigAssociationId
              )
          ]
      )

instance
  Data.ToPath
    GetResolverQueryLogConfigAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetResolverQueryLogConfigAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverQueryLogConfigAssociationResponse' smart constructor.
data GetResolverQueryLogConfigAssociationResponse = GetResolverQueryLogConfigAssociationResponse'
  { -- | Information about the Resolver query logging configuration association
    -- that you specified in a @GetQueryLogConfigAssociation@ request.
    resolverQueryLogConfigAssociation :: Prelude.Maybe ResolverQueryLogConfigAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverQueryLogConfigAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigAssociation', 'getResolverQueryLogConfigAssociationResponse_resolverQueryLogConfigAssociation' - Information about the Resolver query logging configuration association
-- that you specified in a @GetQueryLogConfigAssociation@ request.
--
-- 'httpStatus', 'getResolverQueryLogConfigAssociationResponse_httpStatus' - The response's http status code.
newGetResolverQueryLogConfigAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverQueryLogConfigAssociationResponse
newGetResolverQueryLogConfigAssociationResponse
  pHttpStatus_ =
    GetResolverQueryLogConfigAssociationResponse'
      { resolverQueryLogConfigAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the Resolver query logging configuration association
-- that you specified in a @GetQueryLogConfigAssociation@ request.
getResolverQueryLogConfigAssociationResponse_resolverQueryLogConfigAssociation :: Lens.Lens' GetResolverQueryLogConfigAssociationResponse (Prelude.Maybe ResolverQueryLogConfigAssociation)
getResolverQueryLogConfigAssociationResponse_resolverQueryLogConfigAssociation = Lens.lens (\GetResolverQueryLogConfigAssociationResponse' {resolverQueryLogConfigAssociation} -> resolverQueryLogConfigAssociation) (\s@GetResolverQueryLogConfigAssociationResponse' {} a -> s {resolverQueryLogConfigAssociation = a} :: GetResolverQueryLogConfigAssociationResponse)

-- | The response's http status code.
getResolverQueryLogConfigAssociationResponse_httpStatus :: Lens.Lens' GetResolverQueryLogConfigAssociationResponse Prelude.Int
getResolverQueryLogConfigAssociationResponse_httpStatus = Lens.lens (\GetResolverQueryLogConfigAssociationResponse' {httpStatus} -> httpStatus) (\s@GetResolverQueryLogConfigAssociationResponse' {} a -> s {httpStatus = a} :: GetResolverQueryLogConfigAssociationResponse)

instance
  Prelude.NFData
    GetResolverQueryLogConfigAssociationResponse
  where
  rnf GetResolverQueryLogConfigAssociationResponse' {..} =
    Prelude.rnf resolverQueryLogConfigAssociation
      `Prelude.seq` Prelude.rnf httpStatus
