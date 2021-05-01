{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNetworkInsightsPath' smart constructor.
data CreateNetworkInsightsPath = CreateNetworkInsightsPath'
  { -- | The tags to add to the path.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IP address of the AWS resource that is the destination of the path.
    destinationIp :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the AWS resource that is the source of the path.
    sourceIp :: Prelude.Maybe Prelude.Text,
    -- | The destination port.
    destinationPort :: Prelude.Maybe Prelude.Natural,
    -- | The AWS resource that is the source of the path.
    source :: Prelude.Text,
    -- | The AWS resource that is the destination of the path.
    destination :: Prelude.Text,
    -- | The protocol.
    protocol :: Protocol,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'destination'
  Prelude.Text ->
  -- | 'protocol'
  Protocol ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateNetworkInsightsPath
newCreateNetworkInsightsPath
  pSource_
  pDestination_
  pProtocol_
  pClientToken_ =
    CreateNetworkInsightsPath'
      { tagSpecifications =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        destinationIp = Prelude.Nothing,
        sourceIp = Prelude.Nothing,
        destinationPort = Prelude.Nothing,
        source = pSource_,
        destination = pDestination_,
        protocol = pProtocol_,
        clientToken = pClientToken_
      }

-- | The tags to add to the path.
createNetworkInsightsPath_tagSpecifications :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe [TagSpecification])
createNetworkInsightsPath_tagSpecifications = Lens.lens (\CreateNetworkInsightsPath' {tagSpecifications} -> tagSpecifications) (\s@CreateNetworkInsightsPath' {} a -> s {tagSpecifications = a} :: CreateNetworkInsightsPath) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInsightsPath_dryRun :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Bool)
createNetworkInsightsPath_dryRun = Lens.lens (\CreateNetworkInsightsPath' {dryRun} -> dryRun) (\s@CreateNetworkInsightsPath' {} a -> s {dryRun = a} :: CreateNetworkInsightsPath)

-- | The IP address of the AWS resource that is the destination of the path.
createNetworkInsightsPath_destinationIp :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Text)
createNetworkInsightsPath_destinationIp = Lens.lens (\CreateNetworkInsightsPath' {destinationIp} -> destinationIp) (\s@CreateNetworkInsightsPath' {} a -> s {destinationIp = a} :: CreateNetworkInsightsPath)

-- | The IP address of the AWS resource that is the source of the path.
createNetworkInsightsPath_sourceIp :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Text)
createNetworkInsightsPath_sourceIp = Lens.lens (\CreateNetworkInsightsPath' {sourceIp} -> sourceIp) (\s@CreateNetworkInsightsPath' {} a -> s {sourceIp = a} :: CreateNetworkInsightsPath)

-- | The destination port.
createNetworkInsightsPath_destinationPort :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Natural)
createNetworkInsightsPath_destinationPort = Lens.lens (\CreateNetworkInsightsPath' {destinationPort} -> destinationPort) (\s@CreateNetworkInsightsPath' {} a -> s {destinationPort = a} :: CreateNetworkInsightsPath)

-- | The AWS resource that is the source of the path.
createNetworkInsightsPath_source :: Lens.Lens' CreateNetworkInsightsPath Prelude.Text
createNetworkInsightsPath_source = Lens.lens (\CreateNetworkInsightsPath' {source} -> source) (\s@CreateNetworkInsightsPath' {} a -> s {source = a} :: CreateNetworkInsightsPath)

-- | The AWS resource that is the destination of the path.
createNetworkInsightsPath_destination :: Lens.Lens' CreateNetworkInsightsPath Prelude.Text
createNetworkInsightsPath_destination = Lens.lens (\CreateNetworkInsightsPath' {destination} -> destination) (\s@CreateNetworkInsightsPath' {} a -> s {destination = a} :: CreateNetworkInsightsPath)

-- | The protocol.
createNetworkInsightsPath_protocol :: Lens.Lens' CreateNetworkInsightsPath Protocol
createNetworkInsightsPath_protocol = Lens.lens (\CreateNetworkInsightsPath' {protocol} -> protocol) (\s@CreateNetworkInsightsPath' {} a -> s {protocol = a} :: CreateNetworkInsightsPath)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createNetworkInsightsPath_clientToken :: Lens.Lens' CreateNetworkInsightsPath Prelude.Text
createNetworkInsightsPath_clientToken = Lens.lens (\CreateNetworkInsightsPath' {clientToken} -> clientToken) (\s@CreateNetworkInsightsPath' {} a -> s {clientToken = a} :: CreateNetworkInsightsPath)

instance Prelude.AWSRequest CreateNetworkInsightsPath where
  type
    Rs CreateNetworkInsightsPath =
      CreateNetworkInsightsPathResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInsightsPathResponse'
            Prelude.<$> (x Prelude..@? "networkInsightsPath")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkInsightsPath

instance Prelude.NFData CreateNetworkInsightsPath

instance Prelude.ToHeaders CreateNetworkInsightsPath where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateNetworkInsightsPath where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateNetworkInsightsPath where
  toQuery CreateNetworkInsightsPath' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateNetworkInsightsPath" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "DestinationIp" Prelude.=: destinationIp,
        "SourceIp" Prelude.=: sourceIp,
        "DestinationPort" Prelude.=: destinationPort,
        "Source" Prelude.=: source,
        "Destination" Prelude.=: destination,
        "Protocol" Prelude.=: protocol,
        "ClientToken" Prelude.=: clientToken
      ]

-- | /See:/ 'newCreateNetworkInsightsPathResponse' smart constructor.
data CreateNetworkInsightsPathResponse = CreateNetworkInsightsPathResponse'
  { -- | Information about the path.
    networkInsightsPath :: Prelude.Maybe NetworkInsightsPath,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateNetworkInsightsPathResponse
newCreateNetworkInsightsPathResponse pHttpStatus_ =
  CreateNetworkInsightsPathResponse'
    { networkInsightsPath =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the path.
createNetworkInsightsPathResponse_networkInsightsPath :: Lens.Lens' CreateNetworkInsightsPathResponse (Prelude.Maybe NetworkInsightsPath)
createNetworkInsightsPathResponse_networkInsightsPath = Lens.lens (\CreateNetworkInsightsPathResponse' {networkInsightsPath} -> networkInsightsPath) (\s@CreateNetworkInsightsPathResponse' {} a -> s {networkInsightsPath = a} :: CreateNetworkInsightsPathResponse)

-- | The response's http status code.
createNetworkInsightsPathResponse_httpStatus :: Lens.Lens' CreateNetworkInsightsPathResponse Prelude.Int
createNetworkInsightsPathResponse_httpStatus = Lens.lens (\CreateNetworkInsightsPathResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkInsightsPathResponse' {} a -> s {httpStatus = a} :: CreateNetworkInsightsPathResponse)

instance
  Prelude.NFData
    CreateNetworkInsightsPathResponse
