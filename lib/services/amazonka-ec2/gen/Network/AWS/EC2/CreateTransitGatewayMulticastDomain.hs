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
-- Module      : Network.AWS.EC2.CreateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multicast domain using the specified transit gateway.
--
-- The transit gateway must be in the available state before you create a
-- domain. Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGateways.html DescribeTransitGateways>
-- to see the state of transit gateway.
module Network.AWS.EC2.CreateTransitGatewayMulticastDomain
  ( -- * Creating a Request
    CreateTransitGatewayMulticastDomain (..),
    newCreateTransitGatewayMulticastDomain,

    -- * Request Lenses
    createTransitGatewayMulticastDomain_tagSpecifications,
    createTransitGatewayMulticastDomain_options,
    createTransitGatewayMulticastDomain_dryRun,
    createTransitGatewayMulticastDomain_transitGatewayId,

    -- * Destructuring the Response
    CreateTransitGatewayMulticastDomainResponse (..),
    newCreateTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    createTransitGatewayMulticastDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGatewayMulticastDomain' smart constructor.
data CreateTransitGatewayMulticastDomain = CreateTransitGatewayMulticastDomain'
  { -- | The tags for the transit gateway multicast domain.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The options for the transit gateway multicast domain.
    options :: Prelude.Maybe CreateTransitGatewayMulticastDomainRequestOptions,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayMulticastDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTransitGatewayMulticastDomain_tagSpecifications' - The tags for the transit gateway multicast domain.
--
-- 'options', 'createTransitGatewayMulticastDomain_options' - The options for the transit gateway multicast domain.
--
-- 'dryRun', 'createTransitGatewayMulticastDomain_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayId', 'createTransitGatewayMulticastDomain_transitGatewayId' - The ID of the transit gateway.
newCreateTransitGatewayMulticastDomain ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  CreateTransitGatewayMulticastDomain
newCreateTransitGatewayMulticastDomain
  pTransitGatewayId_ =
    CreateTransitGatewayMulticastDomain'
      { tagSpecifications =
          Prelude.Nothing,
        options = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        transitGatewayId = pTransitGatewayId_
      }

-- | The tags for the transit gateway multicast domain.
createTransitGatewayMulticastDomain_tagSpecifications :: Lens.Lens' CreateTransitGatewayMulticastDomain (Prelude.Maybe [TagSpecification])
createTransitGatewayMulticastDomain_tagSpecifications = Lens.lens (\CreateTransitGatewayMulticastDomain' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayMulticastDomain) Prelude.. Lens.mapping Lens.coerced

-- | The options for the transit gateway multicast domain.
createTransitGatewayMulticastDomain_options :: Lens.Lens' CreateTransitGatewayMulticastDomain (Prelude.Maybe CreateTransitGatewayMulticastDomainRequestOptions)
createTransitGatewayMulticastDomain_options = Lens.lens (\CreateTransitGatewayMulticastDomain' {options} -> options) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {options = a} :: CreateTransitGatewayMulticastDomain)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayMulticastDomain_dryRun :: Lens.Lens' CreateTransitGatewayMulticastDomain (Prelude.Maybe Prelude.Bool)
createTransitGatewayMulticastDomain_dryRun = Lens.lens (\CreateTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: CreateTransitGatewayMulticastDomain)

-- | The ID of the transit gateway.
createTransitGatewayMulticastDomain_transitGatewayId :: Lens.Lens' CreateTransitGatewayMulticastDomain Prelude.Text
createTransitGatewayMulticastDomain_transitGatewayId = Lens.lens (\CreateTransitGatewayMulticastDomain' {transitGatewayId} -> transitGatewayId) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {transitGatewayId = a} :: CreateTransitGatewayMulticastDomain)

instance
  Core.AWSRequest
    CreateTransitGatewayMulticastDomain
  where
  type
    AWSResponse CreateTransitGatewayMulticastDomain =
      CreateTransitGatewayMulticastDomainResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayMulticastDomainResponse'
            Prelude.<$> (x Core..@? "transitGatewayMulticastDomain")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayMulticastDomain

instance
  Prelude.NFData
    CreateTransitGatewayMulticastDomain

instance
  Core.ToHeaders
    CreateTransitGatewayMulticastDomain
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    CreateTransitGatewayMulticastDomain
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateTransitGatewayMulticastDomain
  where
  toQuery CreateTransitGatewayMulticastDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateTransitGatewayMulticastDomain" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Options" Core.=: options,
        "DryRun" Core.=: dryRun,
        "TransitGatewayId" Core.=: transitGatewayId
      ]

-- | /See:/ 'newCreateTransitGatewayMulticastDomainResponse' smart constructor.
data CreateTransitGatewayMulticastDomainResponse = CreateTransitGatewayMulticastDomainResponse'
  { -- | Information about the transit gateway multicast domain.
    transitGatewayMulticastDomain :: Prelude.Maybe TransitGatewayMulticastDomain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayMulticastDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomain', 'createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain' - Information about the transit gateway multicast domain.
--
-- 'httpStatus', 'createTransitGatewayMulticastDomainResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayMulticastDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayMulticastDomainResponse
newCreateTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    CreateTransitGatewayMulticastDomainResponse'
      { transitGatewayMulticastDomain =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the transit gateway multicast domain.
createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse (Prelude.Maybe TransitGatewayMulticastDomain)
createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain = Lens.lens (\CreateTransitGatewayMulticastDomainResponse' {transitGatewayMulticastDomain} -> transitGatewayMulticastDomain) (\s@CreateTransitGatewayMulticastDomainResponse' {} a -> s {transitGatewayMulticastDomain = a} :: CreateTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
createTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse Prelude.Int
createTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\CreateTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayMulticastDomainResponse)

instance
  Prelude.NFData
    CreateTransitGatewayMulticastDomainResponse
