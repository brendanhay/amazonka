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
-- Module      : Amazonka.EC2.CreateNetworkInsightsPath
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.CreateNetworkInsightsPath
  ( -- * Creating a Request
    CreateNetworkInsightsPath (..),
    newCreateNetworkInsightsPath,

    -- * Request Lenses
    createNetworkInsightsPath_destinationIp,
    createNetworkInsightsPath_sourceIp,
    createNetworkInsightsPath_destinationPort,
    createNetworkInsightsPath_dryRun,
    createNetworkInsightsPath_tagSpecifications,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkInsightsPath' smart constructor.
data CreateNetworkInsightsPath = CreateNetworkInsightsPath'
  { -- | The IP address of the Amazon Web Services resource that is the
    -- destination of the path.
    destinationIp :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the Amazon Web Services resource that is the source of
    -- the path.
    sourceIp :: Prelude.Maybe Prelude.Text,
    -- | The destination port.
    destinationPort :: Prelude.Maybe Prelude.Natural,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to add to the path.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The Amazon Web Services resource that is the source of the path.
    source :: Prelude.Text,
    -- | The Amazon Web Services resource that is the destination of the path.
    destination :: Prelude.Text,
    -- | The protocol.
    protocol :: Protocol,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInsightsPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationIp', 'createNetworkInsightsPath_destinationIp' - The IP address of the Amazon Web Services resource that is the
-- destination of the path.
--
-- 'sourceIp', 'createNetworkInsightsPath_sourceIp' - The IP address of the Amazon Web Services resource that is the source of
-- the path.
--
-- 'destinationPort', 'createNetworkInsightsPath_destinationPort' - The destination port.
--
-- 'dryRun', 'createNetworkInsightsPath_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createNetworkInsightsPath_tagSpecifications' - The tags to add to the path.
--
-- 'source', 'createNetworkInsightsPath_source' - The Amazon Web Services resource that is the source of the path.
--
-- 'destination', 'createNetworkInsightsPath_destination' - The Amazon Web Services resource that is the destination of the path.
--
-- 'protocol', 'createNetworkInsightsPath_protocol' - The protocol.
--
-- 'clientToken', 'createNetworkInsightsPath_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
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
      { destinationIp =
          Prelude.Nothing,
        sourceIp = Prelude.Nothing,
        destinationPort = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        source = pSource_,
        destination = pDestination_,
        protocol = pProtocol_,
        clientToken = pClientToken_
      }

-- | The IP address of the Amazon Web Services resource that is the
-- destination of the path.
createNetworkInsightsPath_destinationIp :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Text)
createNetworkInsightsPath_destinationIp = Lens.lens (\CreateNetworkInsightsPath' {destinationIp} -> destinationIp) (\s@CreateNetworkInsightsPath' {} a -> s {destinationIp = a} :: CreateNetworkInsightsPath)

-- | The IP address of the Amazon Web Services resource that is the source of
-- the path.
createNetworkInsightsPath_sourceIp :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Text)
createNetworkInsightsPath_sourceIp = Lens.lens (\CreateNetworkInsightsPath' {sourceIp} -> sourceIp) (\s@CreateNetworkInsightsPath' {} a -> s {sourceIp = a} :: CreateNetworkInsightsPath)

-- | The destination port.
createNetworkInsightsPath_destinationPort :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Natural)
createNetworkInsightsPath_destinationPort = Lens.lens (\CreateNetworkInsightsPath' {destinationPort} -> destinationPort) (\s@CreateNetworkInsightsPath' {} a -> s {destinationPort = a} :: CreateNetworkInsightsPath)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInsightsPath_dryRun :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe Prelude.Bool)
createNetworkInsightsPath_dryRun = Lens.lens (\CreateNetworkInsightsPath' {dryRun} -> dryRun) (\s@CreateNetworkInsightsPath' {} a -> s {dryRun = a} :: CreateNetworkInsightsPath)

-- | The tags to add to the path.
createNetworkInsightsPath_tagSpecifications :: Lens.Lens' CreateNetworkInsightsPath (Prelude.Maybe [TagSpecification])
createNetworkInsightsPath_tagSpecifications = Lens.lens (\CreateNetworkInsightsPath' {tagSpecifications} -> tagSpecifications) (\s@CreateNetworkInsightsPath' {} a -> s {tagSpecifications = a} :: CreateNetworkInsightsPath) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services resource that is the source of the path.
createNetworkInsightsPath_source :: Lens.Lens' CreateNetworkInsightsPath Prelude.Text
createNetworkInsightsPath_source = Lens.lens (\CreateNetworkInsightsPath' {source} -> source) (\s@CreateNetworkInsightsPath' {} a -> s {source = a} :: CreateNetworkInsightsPath)

-- | The Amazon Web Services resource that is the destination of the path.
createNetworkInsightsPath_destination :: Lens.Lens' CreateNetworkInsightsPath Prelude.Text
createNetworkInsightsPath_destination = Lens.lens (\CreateNetworkInsightsPath' {destination} -> destination) (\s@CreateNetworkInsightsPath' {} a -> s {destination = a} :: CreateNetworkInsightsPath)

-- | The protocol.
createNetworkInsightsPath_protocol :: Lens.Lens' CreateNetworkInsightsPath Protocol
createNetworkInsightsPath_protocol = Lens.lens (\CreateNetworkInsightsPath' {protocol} -> protocol) (\s@CreateNetworkInsightsPath' {} a -> s {protocol = a} :: CreateNetworkInsightsPath)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createNetworkInsightsPath_clientToken :: Lens.Lens' CreateNetworkInsightsPath Prelude.Text
createNetworkInsightsPath_clientToken = Lens.lens (\CreateNetworkInsightsPath' {clientToken} -> clientToken) (\s@CreateNetworkInsightsPath' {} a -> s {clientToken = a} :: CreateNetworkInsightsPath)

instance Core.AWSRequest CreateNetworkInsightsPath where
  type
    AWSResponse CreateNetworkInsightsPath =
      CreateNetworkInsightsPathResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInsightsPathResponse'
            Prelude.<$> (x Data..@? "networkInsightsPath")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkInsightsPath where
  hashWithSalt _salt CreateNetworkInsightsPath' {..} =
    _salt `Prelude.hashWithSalt` destinationIp
      `Prelude.hashWithSalt` sourceIp
      `Prelude.hashWithSalt` destinationPort
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateNetworkInsightsPath where
  rnf CreateNetworkInsightsPath' {..} =
    Prelude.rnf destinationIp
      `Prelude.seq` Prelude.rnf sourceIp
      `Prelude.seq` Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateNetworkInsightsPath where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateNetworkInsightsPath where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNetworkInsightsPath where
  toQuery CreateNetworkInsightsPath' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateNetworkInsightsPath" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DestinationIp" Data.=: destinationIp,
        "SourceIp" Data.=: sourceIp,
        "DestinationPort" Data.=: destinationPort,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Source" Data.=: source,
        "Destination" Data.=: destination,
        "Protocol" Data.=: protocol,
        "ClientToken" Data.=: clientToken
      ]

-- | /See:/ 'newCreateNetworkInsightsPathResponse' smart constructor.
data CreateNetworkInsightsPathResponse = CreateNetworkInsightsPathResponse'
  { -- | Information about the path.
    networkInsightsPath :: Prelude.Maybe NetworkInsightsPath,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateNetworkInsightsPathResponse' {..} =
    Prelude.rnf networkInsightsPath
      `Prelude.seq` Prelude.rnf httpStatus
