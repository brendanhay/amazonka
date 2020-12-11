{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a link aggregation group (LAG) with the specified number of bundled physical dedicated connections between the customer network and a specific AWS Direct Connect location. A LAG is a logical interface that uses the Link Aggregation Control Protocol (LACP) to aggregate multiple interfaces, enabling you to treat them as a single interface.
--
-- All connections in a LAG must use the same bandwidth (either 1Gbps or 10Gbps) and must terminate at the same AWS Direct Connect endpoint.
-- You can have up to 10 dedicated connections per LAG. Regardless of this limit, if you request more connections for the LAG than AWS Direct Connect can allocate on a single endpoint, no LAG is created.
-- You can specify an existing physical dedicated connection or interconnect to include in the LAG (which counts towards the total number of connections). Doing so interrupts the current physical dedicated connection, and re-establishes them as a member of the LAG. The LAG will be created on the same AWS Direct Connect endpoint to which the dedicated connection terminates. Any virtual interfaces associated with the dedicated connection are automatically disassociated and re-associated with the LAG. The connection ID does not change.
-- If the AWS account used to create a LAG is a registered AWS Direct Connect Partner, the LAG is automatically enabled to host sub-connections. For a LAG owned by a partner, any associated virtual interfaces cannot be directly configured.
module Network.AWS.DirectConnect.CreateLag
  ( -- * Creating a request
    CreateLag (..),
    mkCreateLag,

    -- ** Request lenses
    clChildConnectionTags,
    clConnectionId,
    clProviderName,
    clTags,
    clNumberOfConnections,
    clLocation,
    clConnectionsBandwidth,
    clLagName,

    -- * Destructuring the response
    Lag (..),
    mkLag,

    -- ** Response lenses
    lagLagId,
    lagConnectionsBandwidth,
    lagMinimumLinks,
    lagLagName,
    lagLocation,
    lagConnections,
    lagAwsDevice,
    lagHasLogicalRedundancy,
    lagAllowsHostedConnections,
    lagNumberOfConnections,
    lagJumboFrameCapable,
    lagLagState,
    lagOwnerAccount,
    lagRegion,
    lagProviderName,
    lagAwsDeviceV2,
    lagTags,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLag' smart constructor.
data CreateLag = CreateLag'
  { childConnectionTags ::
      Lude.Maybe (Lude.NonEmpty Tag),
    connectionId :: Lude.Maybe Lude.Text,
    providerName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    numberOfConnections :: Lude.Int,
    location :: Lude.Text,
    connectionsBandwidth :: Lude.Text,
    lagName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLag' with the minimum fields required to make a request.
--
-- * 'childConnectionTags' - The tags to associate with the automtically created LAGs.
-- * 'connectionId' - The ID of an existing dedicated connection to migrate to the LAG.
-- * 'connectionsBandwidth' - The bandwidth of the individual physical dedicated connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
-- * 'lagName' - The name of the LAG.
-- * 'location' - The location for the LAG.
-- * 'numberOfConnections' - The number of physical dedicated connections initially provisioned and bundled by the LAG.
-- * 'providerName' - The name of the service provider associated with the LAG.
-- * 'tags' - The tags to associate with the LAG.
mkCreateLag ::
  -- | 'numberOfConnections'
  Lude.Int ->
  -- | 'location'
  Lude.Text ->
  -- | 'connectionsBandwidth'
  Lude.Text ->
  -- | 'lagName'
  Lude.Text ->
  CreateLag
mkCreateLag
  pNumberOfConnections_
  pLocation_
  pConnectionsBandwidth_
  pLagName_ =
    CreateLag'
      { childConnectionTags = Lude.Nothing,
        connectionId = Lude.Nothing,
        providerName = Lude.Nothing,
        tags = Lude.Nothing,
        numberOfConnections = pNumberOfConnections_,
        location = pLocation_,
        connectionsBandwidth = pConnectionsBandwidth_,
        lagName = pLagName_
      }

-- | The tags to associate with the automtically created LAGs.
--
-- /Note:/ Consider using 'childConnectionTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clChildConnectionTags :: Lens.Lens' CreateLag (Lude.Maybe (Lude.NonEmpty Tag))
clChildConnectionTags = Lens.lens (childConnectionTags :: CreateLag -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {childConnectionTags = a} :: CreateLag)
{-# DEPRECATED clChildConnectionTags "Use generic-lens or generic-optics with 'childConnectionTags' instead." #-}

-- | The ID of an existing dedicated connection to migrate to the LAG.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConnectionId :: Lens.Lens' CreateLag (Lude.Maybe Lude.Text)
clConnectionId = Lens.lens (connectionId :: CreateLag -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: CreateLag)
{-# DEPRECATED clConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The name of the service provider associated with the LAG.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clProviderName :: Lens.Lens' CreateLag (Lude.Maybe Lude.Text)
clProviderName = Lens.lens (providerName :: CreateLag -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: CreateLag)
{-# DEPRECATED clProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The tags to associate with the LAG.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clTags :: Lens.Lens' CreateLag (Lude.Maybe (Lude.NonEmpty Tag))
clTags = Lens.lens (tags :: CreateLag -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateLag)
{-# DEPRECATED clTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The number of physical dedicated connections initially provisioned and bundled by the LAG.
--
-- /Note:/ Consider using 'numberOfConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clNumberOfConnections :: Lens.Lens' CreateLag Lude.Int
clNumberOfConnections = Lens.lens (numberOfConnections :: CreateLag -> Lude.Int) (\s a -> s {numberOfConnections = a} :: CreateLag)
{-# DEPRECATED clNumberOfConnections "Use generic-lens or generic-optics with 'numberOfConnections' instead." #-}

-- | The location for the LAG.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLocation :: Lens.Lens' CreateLag Lude.Text
clLocation = Lens.lens (location :: CreateLag -> Lude.Text) (\s a -> s {location = a} :: CreateLag)
{-# DEPRECATED clLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The bandwidth of the individual physical dedicated connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
--
-- /Note:/ Consider using 'connectionsBandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConnectionsBandwidth :: Lens.Lens' CreateLag Lude.Text
clConnectionsBandwidth = Lens.lens (connectionsBandwidth :: CreateLag -> Lude.Text) (\s a -> s {connectionsBandwidth = a} :: CreateLag)
{-# DEPRECATED clConnectionsBandwidth "Use generic-lens or generic-optics with 'connectionsBandwidth' instead." #-}

-- | The name of the LAG.
--
-- /Note:/ Consider using 'lagName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLagName :: Lens.Lens' CreateLag Lude.Text
clLagName = Lens.lens (lagName :: CreateLag -> Lude.Text) (\s a -> s {lagName = a} :: CreateLag)
{-# DEPRECATED clLagName "Use generic-lens or generic-optics with 'lagName' instead." #-}

instance Lude.AWSRequest CreateLag where
  type Rs CreateLag = Lag
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateLag where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.CreateLag" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLag where
  toJSON CreateLag' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("childConnectionTags" Lude..=) Lude.<$> childConnectionTags,
            ("connectionId" Lude..=) Lude.<$> connectionId,
            ("providerName" Lude..=) Lude.<$> providerName,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("numberOfConnections" Lude..= numberOfConnections),
            Lude.Just ("location" Lude..= location),
            Lude.Just ("connectionsBandwidth" Lude..= connectionsBandwidth),
            Lude.Just ("lagName" Lude..= lagName)
          ]
      )

instance Lude.ToPath CreateLag where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLag where
  toQuery = Lude.const Lude.mempty
