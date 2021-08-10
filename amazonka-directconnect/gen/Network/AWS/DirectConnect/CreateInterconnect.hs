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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInterconnect' smart constructor.
data CreateInterconnect = CreateInterconnect'
  { -- | The name of the service provider associated with the interconnect.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The tags to associate with the interconnect.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the interconnect.
    interconnectName :: Prelude.Text,
    -- | The port bandwidth, in Gbps. The possible values are 1 and 10.
    bandwidth :: Prelude.Text,
    -- | The location of the interconnect.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'bandwidth'
  Prelude.Text ->
  -- | 'location'
  Prelude.Text ->
  CreateInterconnect
newCreateInterconnect
  pInterconnectName_
  pBandwidth_
  pLocation_ =
    CreateInterconnect'
      { providerName = Prelude.Nothing,
        lagId = Prelude.Nothing,
        tags = Prelude.Nothing,
        interconnectName = pInterconnectName_,
        bandwidth = pBandwidth_,
        location = pLocation_
      }

-- | The name of the service provider associated with the interconnect.
createInterconnect_providerName :: Lens.Lens' CreateInterconnect (Prelude.Maybe Prelude.Text)
createInterconnect_providerName = Lens.lens (\CreateInterconnect' {providerName} -> providerName) (\s@CreateInterconnect' {} a -> s {providerName = a} :: CreateInterconnect)

-- | The ID of the LAG.
createInterconnect_lagId :: Lens.Lens' CreateInterconnect (Prelude.Maybe Prelude.Text)
createInterconnect_lagId = Lens.lens (\CreateInterconnect' {lagId} -> lagId) (\s@CreateInterconnect' {} a -> s {lagId = a} :: CreateInterconnect)

-- | The tags to associate with the interconnect.
createInterconnect_tags :: Lens.Lens' CreateInterconnect (Prelude.Maybe (Prelude.NonEmpty Tag))
createInterconnect_tags = Lens.lens (\CreateInterconnect' {tags} -> tags) (\s@CreateInterconnect' {} a -> s {tags = a} :: CreateInterconnect) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the interconnect.
createInterconnect_interconnectName :: Lens.Lens' CreateInterconnect Prelude.Text
createInterconnect_interconnectName = Lens.lens (\CreateInterconnect' {interconnectName} -> interconnectName) (\s@CreateInterconnect' {} a -> s {interconnectName = a} :: CreateInterconnect)

-- | The port bandwidth, in Gbps. The possible values are 1 and 10.
createInterconnect_bandwidth :: Lens.Lens' CreateInterconnect Prelude.Text
createInterconnect_bandwidth = Lens.lens (\CreateInterconnect' {bandwidth} -> bandwidth) (\s@CreateInterconnect' {} a -> s {bandwidth = a} :: CreateInterconnect)

-- | The location of the interconnect.
createInterconnect_location :: Lens.Lens' CreateInterconnect Prelude.Text
createInterconnect_location = Lens.lens (\CreateInterconnect' {location} -> location) (\s@CreateInterconnect' {} a -> s {location = a} :: CreateInterconnect)

instance Core.AWSRequest CreateInterconnect where
  type AWSResponse CreateInterconnect = Interconnect
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateInterconnect

instance Prelude.NFData CreateInterconnect

instance Core.ToHeaders CreateInterconnect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateInterconnect" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateInterconnect where
  toJSON CreateInterconnect' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("providerName" Core..=) Prelude.<$> providerName,
            ("lagId" Core..=) Prelude.<$> lagId,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("interconnectName" Core..= interconnectName),
            Prelude.Just ("bandwidth" Core..= bandwidth),
            Prelude.Just ("location" Core..= location)
          ]
      )

instance Core.ToPath CreateInterconnect where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateInterconnect where
  toQuery = Prelude.const Prelude.mempty
