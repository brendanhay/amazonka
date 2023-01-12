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
-- Module      : Amazonka.DirectConnect.CreateLag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a link aggregation group (LAG) with the specified number of
-- bundled physical dedicated connections between the customer network and
-- a specific Direct Connect location. A LAG is a logical interface that
-- uses the Link Aggregation Control Protocol (LACP) to aggregate multiple
-- interfaces, enabling you to treat them as a single interface.
--
-- All connections in a LAG must use the same bandwidth (either 1Gbps or
-- 10Gbps) and must terminate at the same Direct Connect endpoint.
--
-- You can have up to 10 dedicated connections per LAG. Regardless of this
-- limit, if you request more connections for the LAG than Direct Connect
-- can allocate on a single endpoint, no LAG is created.
--
-- You can specify an existing physical dedicated connection or
-- interconnect to include in the LAG (which counts towards the total
-- number of connections). Doing so interrupts the current physical
-- dedicated connection, and re-establishes them as a member of the LAG.
-- The LAG will be created on the same Direct Connect endpoint to which the
-- dedicated connection terminates. Any virtual interfaces associated with
-- the dedicated connection are automatically disassociated and
-- re-associated with the LAG. The connection ID does not change.
--
-- If the Amazon Web Services account used to create a LAG is a registered
-- Direct Connect Partner, the LAG is automatically enabled to host
-- sub-connections. For a LAG owned by a partner, any associated virtual
-- interfaces cannot be directly configured.
module Amazonka.DirectConnect.CreateLag
  ( -- * Creating a Request
    CreateLag (..),
    newCreateLag,

    -- * Request Lenses
    createLag_childConnectionTags,
    createLag_connectionId,
    createLag_providerName,
    createLag_requestMACSec,
    createLag_tags,
    createLag_numberOfConnections,
    createLag_location,
    createLag_connectionsBandwidth,
    createLag_lagName,

    -- * Destructuring the Response
    Lag (..),
    newLag,

    -- * Response Lenses
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLag' smart constructor.
data CreateLag = CreateLag'
  { -- | The tags to associate with the automtically created LAGs.
    childConnectionTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of an existing dedicated connection to migrate to the LAG.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider associated with the LAG.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connection will support MAC Security (MACsec).
    --
    -- All connections in the LAG must be capable of supporting MAC Security
    -- (MACsec). For information about MAC Security (MACsec) prerequisties, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-prerequisites MACsec prerequisties>
    -- in the /Direct Connect User Guide/.
    requestMACSec :: Prelude.Maybe Prelude.Bool,
    -- | The tags to associate with the LAG.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The number of physical dedicated connections initially provisioned and
    -- bundled by the LAG.
    numberOfConnections :: Prelude.Int,
    -- | The location for the LAG.
    location :: Prelude.Text,
    -- | The bandwidth of the individual physical dedicated connections bundled
    -- by the LAG. The possible values are 1Gbps and 10Gbps.
    connectionsBandwidth :: Prelude.Text,
    -- | The name of the LAG.
    lagName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childConnectionTags', 'createLag_childConnectionTags' - The tags to associate with the automtically created LAGs.
--
-- 'connectionId', 'createLag_connectionId' - The ID of an existing dedicated connection to migrate to the LAG.
--
-- 'providerName', 'createLag_providerName' - The name of the service provider associated with the LAG.
--
-- 'requestMACSec', 'createLag_requestMACSec' - Indicates whether the connection will support MAC Security (MACsec).
--
-- All connections in the LAG must be capable of supporting MAC Security
-- (MACsec). For information about MAC Security (MACsec) prerequisties, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-prerequisites MACsec prerequisties>
-- in the /Direct Connect User Guide/.
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
  Prelude.Int ->
  -- | 'location'
  Prelude.Text ->
  -- | 'connectionsBandwidth'
  Prelude.Text ->
  -- | 'lagName'
  Prelude.Text ->
  CreateLag
newCreateLag
  pNumberOfConnections_
  pLocation_
  pConnectionsBandwidth_
  pLagName_ =
    CreateLag'
      { childConnectionTags = Prelude.Nothing,
        connectionId = Prelude.Nothing,
        providerName = Prelude.Nothing,
        requestMACSec = Prelude.Nothing,
        tags = Prelude.Nothing,
        numberOfConnections = pNumberOfConnections_,
        location = pLocation_,
        connectionsBandwidth = pConnectionsBandwidth_,
        lagName = pLagName_
      }

-- | The tags to associate with the automtically created LAGs.
createLag_childConnectionTags :: Lens.Lens' CreateLag (Prelude.Maybe (Prelude.NonEmpty Tag))
createLag_childConnectionTags = Lens.lens (\CreateLag' {childConnectionTags} -> childConnectionTags) (\s@CreateLag' {} a -> s {childConnectionTags = a} :: CreateLag) Prelude.. Lens.mapping Lens.coerced

-- | The ID of an existing dedicated connection to migrate to the LAG.
createLag_connectionId :: Lens.Lens' CreateLag (Prelude.Maybe Prelude.Text)
createLag_connectionId = Lens.lens (\CreateLag' {connectionId} -> connectionId) (\s@CreateLag' {} a -> s {connectionId = a} :: CreateLag)

-- | The name of the service provider associated with the LAG.
createLag_providerName :: Lens.Lens' CreateLag (Prelude.Maybe Prelude.Text)
createLag_providerName = Lens.lens (\CreateLag' {providerName} -> providerName) (\s@CreateLag' {} a -> s {providerName = a} :: CreateLag)

-- | Indicates whether the connection will support MAC Security (MACsec).
--
-- All connections in the LAG must be capable of supporting MAC Security
-- (MACsec). For information about MAC Security (MACsec) prerequisties, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-prerequisites MACsec prerequisties>
-- in the /Direct Connect User Guide/.
createLag_requestMACSec :: Lens.Lens' CreateLag (Prelude.Maybe Prelude.Bool)
createLag_requestMACSec = Lens.lens (\CreateLag' {requestMACSec} -> requestMACSec) (\s@CreateLag' {} a -> s {requestMACSec = a} :: CreateLag)

-- | The tags to associate with the LAG.
createLag_tags :: Lens.Lens' CreateLag (Prelude.Maybe (Prelude.NonEmpty Tag))
createLag_tags = Lens.lens (\CreateLag' {tags} -> tags) (\s@CreateLag' {} a -> s {tags = a} :: CreateLag) Prelude.. Lens.mapping Lens.coerced

-- | The number of physical dedicated connections initially provisioned and
-- bundled by the LAG.
createLag_numberOfConnections :: Lens.Lens' CreateLag Prelude.Int
createLag_numberOfConnections = Lens.lens (\CreateLag' {numberOfConnections} -> numberOfConnections) (\s@CreateLag' {} a -> s {numberOfConnections = a} :: CreateLag)

-- | The location for the LAG.
createLag_location :: Lens.Lens' CreateLag Prelude.Text
createLag_location = Lens.lens (\CreateLag' {location} -> location) (\s@CreateLag' {} a -> s {location = a} :: CreateLag)

-- | The bandwidth of the individual physical dedicated connections bundled
-- by the LAG. The possible values are 1Gbps and 10Gbps.
createLag_connectionsBandwidth :: Lens.Lens' CreateLag Prelude.Text
createLag_connectionsBandwidth = Lens.lens (\CreateLag' {connectionsBandwidth} -> connectionsBandwidth) (\s@CreateLag' {} a -> s {connectionsBandwidth = a} :: CreateLag)

-- | The name of the LAG.
createLag_lagName :: Lens.Lens' CreateLag Prelude.Text
createLag_lagName = Lens.lens (\CreateLag' {lagName} -> lagName) (\s@CreateLag' {} a -> s {lagName = a} :: CreateLag)

instance Core.AWSRequest CreateLag where
  type AWSResponse CreateLag = Lag
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateLag where
  hashWithSalt _salt CreateLag' {..} =
    _salt `Prelude.hashWithSalt` childConnectionTags
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` requestMACSec
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` numberOfConnections
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` connectionsBandwidth
      `Prelude.hashWithSalt` lagName

instance Prelude.NFData CreateLag where
  rnf CreateLag' {..} =
    Prelude.rnf childConnectionTags
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf requestMACSec
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf numberOfConnections
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf connectionsBandwidth
      `Prelude.seq` Prelude.rnf lagName

instance Data.ToHeaders CreateLag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("OvertureService.CreateLag" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLag where
  toJSON CreateLag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("childConnectionTags" Data..=)
              Prelude.<$> childConnectionTags,
            ("connectionId" Data..=) Prelude.<$> connectionId,
            ("providerName" Data..=) Prelude.<$> providerName,
            ("requestMACSec" Data..=) Prelude.<$> requestMACSec,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("numberOfConnections" Data..= numberOfConnections),
            Prelude.Just ("location" Data..= location),
            Prelude.Just
              ( "connectionsBandwidth"
                  Data..= connectionsBandwidth
              ),
            Prelude.Just ("lagName" Data..= lagName)
          ]
      )

instance Data.ToPath CreateLag where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLag where
  toQuery = Prelude.const Prelude.mempty
