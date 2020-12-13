{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an interconnect between an AWS Direct Connect Partner's network and a specific AWS Direct Connect location.
--
-- An interconnect is a connection that is capable of hosting other connections. The AWS Direct Connect partner can use an interconnect to provide AWS Direct Connect hosted connections to customers through their own network services. Like a standard connection, an interconnect links the partner's network to an AWS Direct Connect location over a standard Ethernet fiber-optic cable. One end is connected to the partner's router, the other to an AWS Direct Connect router.
-- You can automatically add the new interconnect to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new interconnect is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no interconnect is created.
-- For each end customer, the AWS Direct Connect Partner provisions a connection on their interconnect by calling 'AllocateHostedConnection' . The end customer can then connect to AWS resources by creating a virtual interface on their connection, using the VLAN assigned to them by the AWS Direct Connect Partner.
module Network.AWS.DirectConnect.CreateInterconnect
  ( -- * Creating a request
    CreateInterconnect (..),
    mkCreateInterconnect,

    -- ** Request lenses
    ciLagId,
    ciLocation,
    ciInterconnectName,
    ciBandwidth,
    ciProviderName,
    ciTags,

    -- * Destructuring the response
    Interconnect (..),
    mkInterconnect,

    -- ** Response lenses
    iLagId,
    iInterconnectId,
    iLocation,
    iInterconnectName,
    iAwsDevice,
    iHasLogicalRedundancy,
    iLoaIssueTime,
    iBandwidth,
    iJumboFrameCapable,
    iInterconnectState,
    iRegion,
    iProviderName,
    iAwsDeviceV2,
    iTags,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInterconnect' smart constructor.
data CreateInterconnect = CreateInterconnect'
  { -- | The ID of the LAG.
    lagId :: Lude.Maybe Lude.Text,
    -- | The location of the interconnect.
    location :: Lude.Text,
    -- | The name of the interconnect.
    interconnectName :: Lude.Text,
    -- | The port bandwidth, in Gbps. The possible values are 1 and 10.
    bandwidth :: Lude.Text,
    -- | The name of the service provider associated with the interconnect.
    providerName :: Lude.Maybe Lude.Text,
    -- | The tags to associate with the interconnect.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInterconnect' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG.
-- * 'location' - The location of the interconnect.
-- * 'interconnectName' - The name of the interconnect.
-- * 'bandwidth' - The port bandwidth, in Gbps. The possible values are 1 and 10.
-- * 'providerName' - The name of the service provider associated with the interconnect.
-- * 'tags' - The tags to associate with the interconnect.
mkCreateInterconnect ::
  -- | 'location'
  Lude.Text ->
  -- | 'interconnectName'
  Lude.Text ->
  -- | 'bandwidth'
  Lude.Text ->
  CreateInterconnect
mkCreateInterconnect pLocation_ pInterconnectName_ pBandwidth_ =
  CreateInterconnect'
    { lagId = Lude.Nothing,
      location = pLocation_,
      interconnectName = pInterconnectName_,
      bandwidth = pBandwidth_,
      providerName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLagId :: Lens.Lens' CreateInterconnect (Lude.Maybe Lude.Text)
ciLagId = Lens.lens (lagId :: CreateInterconnect -> Lude.Maybe Lude.Text) (\s a -> s {lagId = a} :: CreateInterconnect)
{-# DEPRECATED ciLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The location of the interconnect.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLocation :: Lens.Lens' CreateInterconnect Lude.Text
ciLocation = Lens.lens (location :: CreateInterconnect -> Lude.Text) (\s a -> s {location = a} :: CreateInterconnect)
{-# DEPRECATED ciLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the interconnect.
--
-- /Note:/ Consider using 'interconnectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInterconnectName :: Lens.Lens' CreateInterconnect Lude.Text
ciInterconnectName = Lens.lens (interconnectName :: CreateInterconnect -> Lude.Text) (\s a -> s {interconnectName = a} :: CreateInterconnect)
{-# DEPRECATED ciInterconnectName "Use generic-lens or generic-optics with 'interconnectName' instead." #-}

-- | The port bandwidth, in Gbps. The possible values are 1 and 10.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBandwidth :: Lens.Lens' CreateInterconnect Lude.Text
ciBandwidth = Lens.lens (bandwidth :: CreateInterconnect -> Lude.Text) (\s a -> s {bandwidth = a} :: CreateInterconnect)
{-# DEPRECATED ciBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | The name of the service provider associated with the interconnect.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciProviderName :: Lens.Lens' CreateInterconnect (Lude.Maybe Lude.Text)
ciProviderName = Lens.lens (providerName :: CreateInterconnect -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: CreateInterconnect)
{-# DEPRECATED ciProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The tags to associate with the interconnect.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CreateInterconnect (Lude.Maybe (Lude.NonEmpty Tag))
ciTags = Lens.lens (tags :: CreateInterconnect -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateInterconnect)
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateInterconnect where
  type Rs CreateInterconnect = Interconnect
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateInterconnect where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.CreateInterconnect" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInterconnect where
  toJSON CreateInterconnect' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lagId" Lude..=) Lude.<$> lagId,
            Lude.Just ("location" Lude..= location),
            Lude.Just ("interconnectName" Lude..= interconnectName),
            Lude.Just ("bandwidth" Lude..= bandwidth),
            ("providerName" Lude..=) Lude.<$> providerName,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateInterconnect where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInterconnect where
  toQuery = Lude.const Lude.mempty
