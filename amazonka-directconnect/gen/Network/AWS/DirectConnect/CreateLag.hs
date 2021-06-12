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
-- Module      : Network.AWS.DirectConnect.CreateLag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a link aggregation group (LAG) with the specified number of
-- bundled physical dedicated connections between the customer network and
-- a specific AWS Direct Connect location. A LAG is a logical interface
-- that uses the Link Aggregation Control Protocol (LACP) to aggregate
-- multiple interfaces, enabling you to treat them as a single interface.
--
-- All connections in a LAG must use the same bandwidth (either 1Gbps or
-- 10Gbps) and must terminate at the same AWS Direct Connect endpoint.
--
-- You can have up to 10 dedicated connections per LAG. Regardless of this
-- limit, if you request more connections for the LAG than AWS Direct
-- Connect can allocate on a single endpoint, no LAG is created.
--
-- You can specify an existing physical dedicated connection or
-- interconnect to include in the LAG (which counts towards the total
-- number of connections). Doing so interrupts the current physical
-- dedicated connection, and re-establishes them as a member of the LAG.
-- The LAG will be created on the same AWS Direct Connect endpoint to which
-- the dedicated connection terminates. Any virtual interfaces associated
-- with the dedicated connection are automatically disassociated and
-- re-associated with the LAG. The connection ID does not change.
--
-- If the AWS account used to create a LAG is a registered AWS Direct
-- Connect Partner, the LAG is automatically enabled to host
-- sub-connections. For a LAG owned by a partner, any associated virtual
-- interfaces cannot be directly configured.
module Network.AWS.DirectConnect.CreateLag
  ( -- * Creating a Request
    CreateLag (..),
    newCreateLag,

    -- * Request Lenses
    createLag_providerName,
    createLag_connectionId,
    createLag_childConnectionTags,
    createLag_tags,
    createLag_numberOfConnections,
    createLag_location,
    createLag_connectionsBandwidth,
    createLag_lagName,

    -- * Destructuring the Response
    Lag (..),
    newLag,

    -- * Response Lenses
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_providerName,
    lag_hasLogicalRedundancy,
    lag_connections,
    lag_awsDevice,
    lag_lagName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLag' smart constructor.
data CreateLag = CreateLag'
  { -- | The name of the service provider associated with the LAG.
    providerName :: Core.Maybe Core.Text,
    -- | The ID of an existing dedicated connection to migrate to the LAG.
    connectionId :: Core.Maybe Core.Text,
    -- | The tags to associate with the automtically created LAGs.
    childConnectionTags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The tags to associate with the LAG.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The number of physical dedicated connections initially provisioned and
    -- bundled by the LAG.
    numberOfConnections :: Core.Int,
    -- | The location for the LAG.
    location :: Core.Text,
    -- | The bandwidth of the individual physical dedicated connections bundled
    -- by the LAG. The possible values are 1Gbps and 10Gbps.
    connectionsBandwidth :: Core.Text,
    -- | The name of the LAG.
    lagName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerName', 'createLag_providerName' - The name of the service provider associated with the LAG.
--
-- 'connectionId', 'createLag_connectionId' - The ID of an existing dedicated connection to migrate to the LAG.
--
-- 'childConnectionTags', 'createLag_childConnectionTags' - The tags to associate with the automtically created LAGs.
--
-- 'tags', 'createLag_tags' - The tags to associate with the LAG.
--
-- 'numberOfConnections', 'createLag_numberOfConnections' - The number of physical dedicated connections initially provisioned and
-- bundled by the LAG.
--
-- 'location', 'createLag_location' - The location for the LAG.
--
-- 'connectionsBandwidth', 'createLag_connectionsBandwidth' - The bandwidth of the individual physical dedicated connections bundled
-- by the LAG. The possible values are 1Gbps and 10Gbps.
--
-- 'lagName', 'createLag_lagName' - The name of the LAG.
newCreateLag ::
  -- | 'numberOfConnections'
  Core.Int ->
  -- | 'location'
  Core.Text ->
  -- | 'connectionsBandwidth'
  Core.Text ->
  -- | 'lagName'
  Core.Text ->
  CreateLag
newCreateLag
  pNumberOfConnections_
  pLocation_
  pConnectionsBandwidth_
  pLagName_ =
    CreateLag'
      { providerName = Core.Nothing,
        connectionId = Core.Nothing,
        childConnectionTags = Core.Nothing,
        tags = Core.Nothing,
        numberOfConnections = pNumberOfConnections_,
        location = pLocation_,
        connectionsBandwidth = pConnectionsBandwidth_,
        lagName = pLagName_
      }

-- | The name of the service provider associated with the LAG.
createLag_providerName :: Lens.Lens' CreateLag (Core.Maybe Core.Text)
createLag_providerName = Lens.lens (\CreateLag' {providerName} -> providerName) (\s@CreateLag' {} a -> s {providerName = a} :: CreateLag)

-- | The ID of an existing dedicated connection to migrate to the LAG.
createLag_connectionId :: Lens.Lens' CreateLag (Core.Maybe Core.Text)
createLag_connectionId = Lens.lens (\CreateLag' {connectionId} -> connectionId) (\s@CreateLag' {} a -> s {connectionId = a} :: CreateLag)

-- | The tags to associate with the automtically created LAGs.
createLag_childConnectionTags :: Lens.Lens' CreateLag (Core.Maybe (Core.NonEmpty Tag))
createLag_childConnectionTags = Lens.lens (\CreateLag' {childConnectionTags} -> childConnectionTags) (\s@CreateLag' {} a -> s {childConnectionTags = a} :: CreateLag) Core.. Lens.mapping Lens._Coerce

-- | The tags to associate with the LAG.
createLag_tags :: Lens.Lens' CreateLag (Core.Maybe (Core.NonEmpty Tag))
createLag_tags = Lens.lens (\CreateLag' {tags} -> tags) (\s@CreateLag' {} a -> s {tags = a} :: CreateLag) Core.. Lens.mapping Lens._Coerce

-- | The number of physical dedicated connections initially provisioned and
-- bundled by the LAG.
createLag_numberOfConnections :: Lens.Lens' CreateLag Core.Int
createLag_numberOfConnections = Lens.lens (\CreateLag' {numberOfConnections} -> numberOfConnections) (\s@CreateLag' {} a -> s {numberOfConnections = a} :: CreateLag)

-- | The location for the LAG.
createLag_location :: Lens.Lens' CreateLag Core.Text
createLag_location = Lens.lens (\CreateLag' {location} -> location) (\s@CreateLag' {} a -> s {location = a} :: CreateLag)

-- | The bandwidth of the individual physical dedicated connections bundled
-- by the LAG. The possible values are 1Gbps and 10Gbps.
createLag_connectionsBandwidth :: Lens.Lens' CreateLag Core.Text
createLag_connectionsBandwidth = Lens.lens (\CreateLag' {connectionsBandwidth} -> connectionsBandwidth) (\s@CreateLag' {} a -> s {connectionsBandwidth = a} :: CreateLag)

-- | The name of the LAG.
createLag_lagName :: Lens.Lens' CreateLag Core.Text
createLag_lagName = Lens.lens (\CreateLag' {lagName} -> lagName) (\s@CreateLag' {} a -> s {lagName = a} :: CreateLag)

instance Core.AWSRequest CreateLag where
  type AWSResponse CreateLag = Lag
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateLag

instance Core.NFData CreateLag

instance Core.ToHeaders CreateLag where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.CreateLag" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateLag where
  toJSON CreateLag' {..} =
    Core.object
      ( Core.catMaybes
          [ ("providerName" Core..=) Core.<$> providerName,
            ("connectionId" Core..=) Core.<$> connectionId,
            ("childConnectionTags" Core..=)
              Core.<$> childConnectionTags,
            ("tags" Core..=) Core.<$> tags,
            Core.Just
              ("numberOfConnections" Core..= numberOfConnections),
            Core.Just ("location" Core..= location),
            Core.Just
              ( "connectionsBandwidth"
                  Core..= connectionsBandwidth
              ),
            Core.Just ("lagName" Core..= lagName)
          ]
      )

instance Core.ToPath CreateLag where
  toPath = Core.const "/"

instance Core.ToQuery CreateLag where
  toQuery = Core.const Core.mempty
