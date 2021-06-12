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
    createTransitGatewayMulticastDomain_dryRun,
    createTransitGatewayMulticastDomain_options,
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGatewayMulticastDomain' smart constructor.
data CreateTransitGatewayMulticastDomain = CreateTransitGatewayMulticastDomain'
  { -- | The tags for the transit gateway multicast domain.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The options for the transit gateway multicast domain.
    options :: Core.Maybe CreateTransitGatewayMulticastDomainRequestOptions,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'dryRun', 'createTransitGatewayMulticastDomain_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'createTransitGatewayMulticastDomain_options' - The options for the transit gateway multicast domain.
--
-- 'transitGatewayId', 'createTransitGatewayMulticastDomain_transitGatewayId' - The ID of the transit gateway.
newCreateTransitGatewayMulticastDomain ::
  -- | 'transitGatewayId'
  Core.Text ->
  CreateTransitGatewayMulticastDomain
newCreateTransitGatewayMulticastDomain
  pTransitGatewayId_ =
    CreateTransitGatewayMulticastDomain'
      { tagSpecifications =
          Core.Nothing,
        dryRun = Core.Nothing,
        options = Core.Nothing,
        transitGatewayId = pTransitGatewayId_
      }

-- | The tags for the transit gateway multicast domain.
createTransitGatewayMulticastDomain_tagSpecifications :: Lens.Lens' CreateTransitGatewayMulticastDomain (Core.Maybe [TagSpecification])
createTransitGatewayMulticastDomain_tagSpecifications = Lens.lens (\CreateTransitGatewayMulticastDomain' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayMulticastDomain) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayMulticastDomain_dryRun :: Lens.Lens' CreateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
createTransitGatewayMulticastDomain_dryRun = Lens.lens (\CreateTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: CreateTransitGatewayMulticastDomain)

-- | The options for the transit gateway multicast domain.
createTransitGatewayMulticastDomain_options :: Lens.Lens' CreateTransitGatewayMulticastDomain (Core.Maybe CreateTransitGatewayMulticastDomainRequestOptions)
createTransitGatewayMulticastDomain_options = Lens.lens (\CreateTransitGatewayMulticastDomain' {options} -> options) (\s@CreateTransitGatewayMulticastDomain' {} a -> s {options = a} :: CreateTransitGatewayMulticastDomain)

-- | The ID of the transit gateway.
createTransitGatewayMulticastDomain_transitGatewayId :: Lens.Lens' CreateTransitGatewayMulticastDomain Core.Text
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
            Core.<$> (x Core..@? "transitGatewayMulticastDomain")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateTransitGatewayMulticastDomain

instance
  Core.NFData
    CreateTransitGatewayMulticastDomain

instance
  Core.ToHeaders
    CreateTransitGatewayMulticastDomain
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    CreateTransitGatewayMulticastDomain
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateTransitGatewayMulticastDomain
  where
  toQuery CreateTransitGatewayMulticastDomain' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "CreateTransitGatewayMulticastDomain" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Options" Core.=: options,
        "TransitGatewayId" Core.=: transitGatewayId
      ]

-- | /See:/ 'newCreateTransitGatewayMulticastDomainResponse' smart constructor.
data CreateTransitGatewayMulticastDomainResponse = CreateTransitGatewayMulticastDomainResponse'
  { -- | Information about the transit gateway multicast domain.
    transitGatewayMulticastDomain :: Core.Maybe TransitGatewayMulticastDomain,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateTransitGatewayMulticastDomainResponse
newCreateTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    CreateTransitGatewayMulticastDomainResponse'
      { transitGatewayMulticastDomain =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the transit gateway multicast domain.
createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse (Core.Maybe TransitGatewayMulticastDomain)
createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain = Lens.lens (\CreateTransitGatewayMulticastDomainResponse' {transitGatewayMulticastDomain} -> transitGatewayMulticastDomain) (\s@CreateTransitGatewayMulticastDomainResponse' {} a -> s {transitGatewayMulticastDomain = a} :: CreateTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
createTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse Core.Int
createTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\CreateTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayMulticastDomainResponse)

instance
  Core.NFData
    CreateTransitGatewayMulticastDomainResponse
