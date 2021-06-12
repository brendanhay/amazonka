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
-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an interconnect between an AWS Direct Connect Partner\'s network
-- and a specific AWS Direct Connect location.
--
-- An interconnect is a connection that is capable of hosting other
-- connections. The AWS Direct Connect partner can use an interconnect to
-- provide AWS Direct Connect hosted connections to customers through their
-- own network services. Like a standard connection, an interconnect links
-- the partner\'s network to an AWS Direct Connect location over a standard
-- Ethernet fiber-optic cable. One end is connected to the partner\'s
-- router, the other to an AWS Direct Connect router.
--
-- You can automatically add the new interconnect to a link aggregation
-- group (LAG) by specifying a LAG ID in the request. This ensures that the
-- new interconnect is allocated on the same AWS Direct Connect endpoint
-- that hosts the specified LAG. If there are no available ports on the
-- endpoint, the request fails and no interconnect is created.
--
-- For each end customer, the AWS Direct Connect Partner provisions a
-- connection on their interconnect by calling AllocateHostedConnection.
-- The end customer can then connect to AWS resources by creating a virtual
-- interface on their connection, using the VLAN assigned to them by the
-- AWS Direct Connect Partner.
--
-- Intended for use by AWS Direct Connect Partners only.
module Network.AWS.DirectConnect.CreateInterconnect
  ( -- * Creating a Request
    CreateInterconnect (..),
    newCreateInterconnect,

    -- * Request Lenses
    createInterconnect_providerName,
    createInterconnect_lagId,
    createInterconnect_tags,
    createInterconnect_interconnectName,
    createInterconnect_bandwidth,
    createInterconnect_location,

    -- * Destructuring the Response
    Interconnect (..),
    newInterconnect,

    -- * Response Lenses
    interconnect_bandwidth,
    interconnect_interconnectId,
    interconnect_awsDeviceV2,
    interconnect_providerName,
    interconnect_hasLogicalRedundancy,
    interconnect_awsDevice,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_tags,
    interconnect_loaIssueTime,
    interconnect_region,
    interconnect_interconnectState,
    interconnect_location,
    interconnect_interconnectName,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInterconnect' smart constructor.
data CreateInterconnect = CreateInterconnect'
  { -- | The name of the service provider associated with the interconnect.
    providerName :: Core.Maybe Core.Text,
    -- | The ID of the LAG.
    lagId :: Core.Maybe Core.Text,
    -- | The tags to associate with the interconnect.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The name of the interconnect.
    interconnectName :: Core.Text,
    -- | The port bandwidth, in Gbps. The possible values are 1 and 10.
    bandwidth :: Core.Text,
    -- | The location of the interconnect.
    location :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInterconnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerName', 'createInterconnect_providerName' - The name of the service provider associated with the interconnect.
--
-- 'lagId', 'createInterconnect_lagId' - The ID of the LAG.
--
-- 'tags', 'createInterconnect_tags' - The tags to associate with the interconnect.
--
-- 'interconnectName', 'createInterconnect_interconnectName' - The name of the interconnect.
--
-- 'bandwidth', 'createInterconnect_bandwidth' - The port bandwidth, in Gbps. The possible values are 1 and 10.
--
-- 'location', 'createInterconnect_location' - The location of the interconnect.
newCreateInterconnect ::
  -- | 'interconnectName'
  Core.Text ->
  -- | 'bandwidth'
  Core.Text ->
  -- | 'location'
  Core.Text ->
  CreateInterconnect
newCreateInterconnect
  pInterconnectName_
  pBandwidth_
  pLocation_ =
    CreateInterconnect'
      { providerName = Core.Nothing,
        lagId = Core.Nothing,
        tags = Core.Nothing,
        interconnectName = pInterconnectName_,
        bandwidth = pBandwidth_,
        location = pLocation_
      }

-- | The name of the service provider associated with the interconnect.
createInterconnect_providerName :: Lens.Lens' CreateInterconnect (Core.Maybe Core.Text)
createInterconnect_providerName = Lens.lens (\CreateInterconnect' {providerName} -> providerName) (\s@CreateInterconnect' {} a -> s {providerName = a} :: CreateInterconnect)

-- | The ID of the LAG.
createInterconnect_lagId :: Lens.Lens' CreateInterconnect (Core.Maybe Core.Text)
createInterconnect_lagId = Lens.lens (\CreateInterconnect' {lagId} -> lagId) (\s@CreateInterconnect' {} a -> s {lagId = a} :: CreateInterconnect)

-- | The tags to associate with the interconnect.
createInterconnect_tags :: Lens.Lens' CreateInterconnect (Core.Maybe (Core.NonEmpty Tag))
createInterconnect_tags = Lens.lens (\CreateInterconnect' {tags} -> tags) (\s@CreateInterconnect' {} a -> s {tags = a} :: CreateInterconnect) Core.. Lens.mapping Lens._Coerce

-- | The name of the interconnect.
createInterconnect_interconnectName :: Lens.Lens' CreateInterconnect Core.Text
createInterconnect_interconnectName = Lens.lens (\CreateInterconnect' {interconnectName} -> interconnectName) (\s@CreateInterconnect' {} a -> s {interconnectName = a} :: CreateInterconnect)

-- | The port bandwidth, in Gbps. The possible values are 1 and 10.
createInterconnect_bandwidth :: Lens.Lens' CreateInterconnect Core.Text
createInterconnect_bandwidth = Lens.lens (\CreateInterconnect' {bandwidth} -> bandwidth) (\s@CreateInterconnect' {} a -> s {bandwidth = a} :: CreateInterconnect)

-- | The location of the interconnect.
createInterconnect_location :: Lens.Lens' CreateInterconnect Core.Text
createInterconnect_location = Lens.lens (\CreateInterconnect' {location} -> location) (\s@CreateInterconnect' {} a -> s {location = a} :: CreateInterconnect)

instance Core.AWSRequest CreateInterconnect where
  type AWSResponse CreateInterconnect = Interconnect
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateInterconnect

instance Core.NFData CreateInterconnect

instance Core.ToHeaders CreateInterconnect where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateInterconnect" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateInterconnect where
  toJSON CreateInterconnect' {..} =
    Core.object
      ( Core.catMaybes
          [ ("providerName" Core..=) Core.<$> providerName,
            ("lagId" Core..=) Core.<$> lagId,
            ("tags" Core..=) Core.<$> tags,
            Core.Just
              ("interconnectName" Core..= interconnectName),
            Core.Just ("bandwidth" Core..= bandwidth),
            Core.Just ("location" Core..= location)
          ]
      )

instance Core.ToPath CreateInterconnect where
  toPath = Core.const "/"

instance Core.ToQuery CreateInterconnect where
  toQuery = Core.const Core.mempty
