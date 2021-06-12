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
-- Module      : Network.AWS.EC2.CreateNetworkInsightsPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a path to analyze for reachability.
--
-- Reachability Analyzer enables you to analyze and debug network
-- reachability between two resources in your virtual private cloud (VPC).
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/ What is Reachability Analyzer>.
module Network.AWS.EC2.CreateNetworkInsightsPath
  ( -- * Creating a Request
    CreateNetworkInsightsPath (..),
    newCreateNetworkInsightsPath,

    -- * Request Lenses
    createNetworkInsightsPath_tagSpecifications,
    createNetworkInsightsPath_dryRun,
    createNetworkInsightsPath_destinationIp,
    createNetworkInsightsPath_sourceIp,
    createNetworkInsightsPath_destinationPort,
    createNetworkInsightsPath_source,
    createNetworkInsightsPath_destination,
    createNetworkInsightsPath_protocol,
    createNetworkInsightsPath_clientToken,

    -- * Destructuring the Response
    CreateNetworkInsightsPathResponse (..),
    newCreateNetworkInsightsPathResponse,

    -- * Response Lenses
    createNetworkInsightsPathResponse_networkInsightsPath,
    createNetworkInsightsPathResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNetworkInsightsPath' smart constructor.
data CreateNetworkInsightsPath = CreateNetworkInsightsPath'
  { -- | The tags to add to the path.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IP address of the AWS resource that is the destination of the path.
    destinationIp :: Core.Maybe Core.Text,
    -- | The IP address of the AWS resource that is the source of the path.
    sourceIp :: Core.Maybe Core.Text,
    -- | The destination port.
    destinationPort :: Core.Maybe Core.Natural,
    -- | The AWS resource that is the source of the path.
    source :: Core.Text,
    -- | The AWS resource that is the destination of the path.
    destination :: Core.Text,
    -- | The protocol.
    protocol :: Protocol,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkInsightsPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createNetworkInsightsPath_tagSpecifications' - The tags to add to the path.
--
-- 'dryRun', 'createNetworkInsightsPath_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'destinationIp', 'createNetworkInsightsPath_destinationIp' - The IP address of the AWS resource that is the destination of the path.
--
-- 'sourceIp', 'createNetworkInsightsPath_sourceIp' - The IP address of the AWS resource that is the source of the path.
--
-- 'destinationPort', 'createNetworkInsightsPath_destinationPort' - The destination port.
--
-- 'source', 'createNetworkInsightsPath_source' - The AWS resource that is the source of the path.
--
-- 'destination', 'createNetworkInsightsPath_destination' - The AWS resource that is the destination of the path.
--
-- 'protocol', 'createNetworkInsightsPath_protocol' - The protocol.
--
-- 'clientToken', 'createNetworkInsightsPath_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
newCreateNetworkInsightsPath ::
  -- | 'source'
  Core.Text ->
  -- | 'destination'
  Core.Text ->
  -- | 'protocol'
  Protocol ->
  -- | 'clientToken'
  Core.Text ->
  CreateNetworkInsightsPath
newCreateNetworkInsightsPath
  pSource_
  pDestination_
  pProtocol_
  pClientToken_ =
    CreateNetworkInsightsPath'
      { tagSpecifications =
          Core.Nothing,
        dryRun = Core.Nothing,
        destinationIp = Core.Nothing,
        sourceIp = Core.Nothing,
        destinationPort = Core.Nothing,
        source = pSource_,
        destination = pDestination_,
        protocol = pProtocol_,
        clientToken = pClientToken_
      }

-- | The tags to add to the path.
createNetworkInsightsPath_tagSpecifications :: Lens.Lens' CreateNetworkInsightsPath (Core.Maybe [TagSpecification])
createNetworkInsightsPath_tagSpecifications = Lens.lens (\CreateNetworkInsightsPath' {tagSpecifications} -> tagSpecifications) (\s@CreateNetworkInsightsPath' {} a -> s {tagSpecifications = a} :: CreateNetworkInsightsPath) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInsightsPath_dryRun :: Lens.Lens' CreateNetworkInsightsPath (Core.Maybe Core.Bool)
createNetworkInsightsPath_dryRun = Lens.lens (\CreateNetworkInsightsPath' {dryRun} -> dryRun) (\s@CreateNetworkInsightsPath' {} a -> s {dryRun = a} :: CreateNetworkInsightsPath)

-- | The IP address of the AWS resource that is the destination of the path.
createNetworkInsightsPath_destinationIp :: Lens.Lens' CreateNetworkInsightsPath (Core.Maybe Core.Text)
createNetworkInsightsPath_destinationIp = Lens.lens (\CreateNetworkInsightsPath' {destinationIp} -> destinationIp) (\s@CreateNetworkInsightsPath' {} a -> s {destinationIp = a} :: CreateNetworkInsightsPath)

-- | The IP address of the AWS resource that is the source of the path.
createNetworkInsightsPath_sourceIp :: Lens.Lens' CreateNetworkInsightsPath (Core.Maybe Core.Text)
createNetworkInsightsPath_sourceIp = Lens.lens (\CreateNetworkInsightsPath' {sourceIp} -> sourceIp) (\s@CreateNetworkInsightsPath' {} a -> s {sourceIp = a} :: CreateNetworkInsightsPath)

-- | The destination port.
createNetworkInsightsPath_destinationPort :: Lens.Lens' CreateNetworkInsightsPath (Core.Maybe Core.Natural)
createNetworkInsightsPath_destinationPort = Lens.lens (\CreateNetworkInsightsPath' {destinationPort} -> destinationPort) (\s@CreateNetworkInsightsPath' {} a -> s {destinationPort = a} :: CreateNetworkInsightsPath)

-- | The AWS resource that is the source of the path.
createNetworkInsightsPath_source :: Lens.Lens' CreateNetworkInsightsPath Core.Text
createNetworkInsightsPath_source = Lens.lens (\CreateNetworkInsightsPath' {source} -> source) (\s@CreateNetworkInsightsPath' {} a -> s {source = a} :: CreateNetworkInsightsPath)

-- | The AWS resource that is the destination of the path.
createNetworkInsightsPath_destination :: Lens.Lens' CreateNetworkInsightsPath Core.Text
createNetworkInsightsPath_destination = Lens.lens (\CreateNetworkInsightsPath' {destination} -> destination) (\s@CreateNetworkInsightsPath' {} a -> s {destination = a} :: CreateNetworkInsightsPath)

-- | The protocol.
createNetworkInsightsPath_protocol :: Lens.Lens' CreateNetworkInsightsPath Protocol
createNetworkInsightsPath_protocol = Lens.lens (\CreateNetworkInsightsPath' {protocol} -> protocol) (\s@CreateNetworkInsightsPath' {} a -> s {protocol = a} :: CreateNetworkInsightsPath)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createNetworkInsightsPath_clientToken :: Lens.Lens' CreateNetworkInsightsPath Core.Text
createNetworkInsightsPath_clientToken = Lens.lens (\CreateNetworkInsightsPath' {clientToken} -> clientToken) (\s@CreateNetworkInsightsPath' {} a -> s {clientToken = a} :: CreateNetworkInsightsPath)

instance Core.AWSRequest CreateNetworkInsightsPath where
  type
    AWSResponse CreateNetworkInsightsPath =
      CreateNetworkInsightsPathResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInsightsPathResponse'
            Core.<$> (x Core..@? "networkInsightsPath")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateNetworkInsightsPath

instance Core.NFData CreateNetworkInsightsPath

instance Core.ToHeaders CreateNetworkInsightsPath where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateNetworkInsightsPath where
  toPath = Core.const "/"

instance Core.ToQuery CreateNetworkInsightsPath where
  toQuery CreateNetworkInsightsPath' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateNetworkInsightsPath" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "DestinationIp" Core.=: destinationIp,
        "SourceIp" Core.=: sourceIp,
        "DestinationPort" Core.=: destinationPort,
        "Source" Core.=: source,
        "Destination" Core.=: destination,
        "Protocol" Core.=: protocol,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newCreateNetworkInsightsPathResponse' smart constructor.
data CreateNetworkInsightsPathResponse = CreateNetworkInsightsPathResponse'
  { -- | Information about the path.
    networkInsightsPath :: Core.Maybe NetworkInsightsPath,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkInsightsPathResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsPath', 'createNetworkInsightsPathResponse_networkInsightsPath' - Information about the path.
--
-- 'httpStatus', 'createNetworkInsightsPathResponse_httpStatus' - The response's http status code.
newCreateNetworkInsightsPathResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateNetworkInsightsPathResponse
newCreateNetworkInsightsPathResponse pHttpStatus_ =
  CreateNetworkInsightsPathResponse'
    { networkInsightsPath =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the path.
createNetworkInsightsPathResponse_networkInsightsPath :: Lens.Lens' CreateNetworkInsightsPathResponse (Core.Maybe NetworkInsightsPath)
createNetworkInsightsPathResponse_networkInsightsPath = Lens.lens (\CreateNetworkInsightsPathResponse' {networkInsightsPath} -> networkInsightsPath) (\s@CreateNetworkInsightsPathResponse' {} a -> s {networkInsightsPath = a} :: CreateNetworkInsightsPathResponse)

-- | The response's http status code.
createNetworkInsightsPathResponse_httpStatus :: Lens.Lens' CreateNetworkInsightsPathResponse Core.Int
createNetworkInsightsPathResponse_httpStatus = Lens.lens (\CreateNetworkInsightsPathResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkInsightsPathResponse' {} a -> s {httpStatus = a} :: CreateNetworkInsightsPathResponse)

instance
  Core.NFData
    CreateNetworkInsightsPathResponse
