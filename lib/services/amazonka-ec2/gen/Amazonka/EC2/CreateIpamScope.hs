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
-- Module      : Amazonka.EC2.CreateIpamScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an IPAM scope. In IPAM, a scope is the highest-level container
-- within IPAM. An IPAM contains two default scopes. Each scope represents
-- the IP space for a single network. The private scope is intended for all
-- private IP address space. The public scope is intended for all public IP
-- address space. Scopes enable you to reuse IP addresses across multiple
-- unconnected networks without causing IP address overlap or conflict.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/add-scope-ipam.html Add a scope>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.CreateIpamScope
  ( -- * Creating a Request
    CreateIpamScope (..),
    newCreateIpamScope,

    -- * Request Lenses
    createIpamScope_clientToken,
    createIpamScope_description,
    createIpamScope_dryRun,
    createIpamScope_tagSpecifications,
    createIpamScope_ipamId,

    -- * Destructuring the Response
    CreateIpamScopeResponse (..),
    newCreateIpamScopeResponse,

    -- * Response Lenses
    createIpamScopeResponse_ipamScope,
    createIpamScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIpamScope' smart constructor.
data CreateIpamScope = CreateIpamScope'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the scope you\'re creating.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the IPAM for which you\'re creating this scope.
    ipamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createIpamScope_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createIpamScope_description' - A description for the scope you\'re creating.
--
-- 'dryRun', 'createIpamScope_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createIpamScope_tagSpecifications' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
--
-- 'ipamId', 'createIpamScope_ipamId' - The ID of the IPAM for which you\'re creating this scope.
newCreateIpamScope ::
  -- | 'ipamId'
  Prelude.Text ->
  CreateIpamScope
newCreateIpamScope pIpamId_ =
  CreateIpamScope'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      ipamId = pIpamId_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createIpamScope_clientToken :: Lens.Lens' CreateIpamScope (Prelude.Maybe Prelude.Text)
createIpamScope_clientToken = Lens.lens (\CreateIpamScope' {clientToken} -> clientToken) (\s@CreateIpamScope' {} a -> s {clientToken = a} :: CreateIpamScope)

-- | A description for the scope you\'re creating.
createIpamScope_description :: Lens.Lens' CreateIpamScope (Prelude.Maybe Prelude.Text)
createIpamScope_description = Lens.lens (\CreateIpamScope' {description} -> description) (\s@CreateIpamScope' {} a -> s {description = a} :: CreateIpamScope)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
createIpamScope_dryRun :: Lens.Lens' CreateIpamScope (Prelude.Maybe Prelude.Bool)
createIpamScope_dryRun = Lens.lens (\CreateIpamScope' {dryRun} -> dryRun) (\s@CreateIpamScope' {} a -> s {dryRun = a} :: CreateIpamScope)

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
createIpamScope_tagSpecifications :: Lens.Lens' CreateIpamScope (Prelude.Maybe [TagSpecification])
createIpamScope_tagSpecifications = Lens.lens (\CreateIpamScope' {tagSpecifications} -> tagSpecifications) (\s@CreateIpamScope' {} a -> s {tagSpecifications = a} :: CreateIpamScope) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the IPAM for which you\'re creating this scope.
createIpamScope_ipamId :: Lens.Lens' CreateIpamScope Prelude.Text
createIpamScope_ipamId = Lens.lens (\CreateIpamScope' {ipamId} -> ipamId) (\s@CreateIpamScope' {} a -> s {ipamId = a} :: CreateIpamScope)

instance Core.AWSRequest CreateIpamScope where
  type
    AWSResponse CreateIpamScope =
      CreateIpamScopeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateIpamScopeResponse'
            Prelude.<$> (x Data..@? "ipamScope")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIpamScope where
  hashWithSalt _salt CreateIpamScope' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` ipamId

instance Prelude.NFData CreateIpamScope where
  rnf CreateIpamScope' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf dryRun `Prelude.seq`
          Prelude.rnf tagSpecifications `Prelude.seq`
            Prelude.rnf ipamId

instance Data.ToHeaders CreateIpamScope where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateIpamScope where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIpamScope where
  toQuery CreateIpamScope' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateIpamScope" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "IpamId" Data.=: ipamId
      ]

-- | /See:/ 'newCreateIpamScopeResponse' smart constructor.
data CreateIpamScopeResponse = CreateIpamScopeResponse'
  { -- | Information about the created scope.
    ipamScope :: Prelude.Maybe IpamScope,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamScope', 'createIpamScopeResponse_ipamScope' - Information about the created scope.
--
-- 'httpStatus', 'createIpamScopeResponse_httpStatus' - The response's http status code.
newCreateIpamScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIpamScopeResponse
newCreateIpamScopeResponse pHttpStatus_ =
  CreateIpamScopeResponse'
    { ipamScope =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the created scope.
createIpamScopeResponse_ipamScope :: Lens.Lens' CreateIpamScopeResponse (Prelude.Maybe IpamScope)
createIpamScopeResponse_ipamScope = Lens.lens (\CreateIpamScopeResponse' {ipamScope} -> ipamScope) (\s@CreateIpamScopeResponse' {} a -> s {ipamScope = a} :: CreateIpamScopeResponse)

-- | The response's http status code.
createIpamScopeResponse_httpStatus :: Lens.Lens' CreateIpamScopeResponse Prelude.Int
createIpamScopeResponse_httpStatus = Lens.lens (\CreateIpamScopeResponse' {httpStatus} -> httpStatus) (\s@CreateIpamScopeResponse' {} a -> s {httpStatus = a} :: CreateIpamScopeResponse)

instance Prelude.NFData CreateIpamScopeResponse where
  rnf CreateIpamScopeResponse' {..} =
    Prelude.rnf ipamScope `Prelude.seq`
      Prelude.rnf httpStatus
