{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection between a customer network and a specific AWS Direct Connect location.
--
-- A connection links your internal network to an AWS Direct Connect location over a standard Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router.
-- To find the locations for your Region, use 'DescribeLocations' .
-- You can automatically add the new connection to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new connection is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no connection is created.
module Network.AWS.DirectConnect.CreateConnection
  ( -- * Creating a request
    CreateConnection (..),
    mkCreateConnection,

    -- ** Request lenses
    ccLagId,
    ccLocation,
    ccConnectionName,
    ccBandwidth,
    ccProviderName,
    ccTags,

    -- * Destructuring the response
    Connection (..),
    mkConnection,

    -- ** Response lenses
    cLagId,
    cVlan,
    cLocation,
    cAwsDevice,
    cHasLogicalRedundancy,
    cConnectionId,
    cLoaIssueTime,
    cPartnerName,
    cConnectionName,
    cBandwidth,
    cJumboFrameCapable,
    cOwnerAccount,
    cRegion,
    cProviderName,
    cAwsDeviceV2,
    cConnectionState,
    cTags,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The ID of the LAG.
    lagId :: Lude.Maybe Lude.Text,
    -- | The location of the connection.
    location :: Lude.Text,
    -- | The name of the connection.
    connectionName :: Lude.Text,
    -- | The bandwidth of the connection.
    bandwidth :: Lude.Text,
    -- | The name of the service provider associated with the requested connection.
    providerName :: Lude.Maybe Lude.Text,
    -- | The tags to associate with the lag.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConnection' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG.
-- * 'location' - The location of the connection.
-- * 'connectionName' - The name of the connection.
-- * 'bandwidth' - The bandwidth of the connection.
-- * 'providerName' - The name of the service provider associated with the requested connection.
-- * 'tags' - The tags to associate with the lag.
mkCreateConnection ::
  -- | 'location'
  Lude.Text ->
  -- | 'connectionName'
  Lude.Text ->
  -- | 'bandwidth'
  Lude.Text ->
  CreateConnection
mkCreateConnection pLocation_ pConnectionName_ pBandwidth_ =
  CreateConnection'
    { lagId = Lude.Nothing,
      location = pLocation_,
      connectionName = pConnectionName_,
      bandwidth = pBandwidth_,
      providerName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLagId :: Lens.Lens' CreateConnection (Lude.Maybe Lude.Text)
ccLagId = Lens.lens (lagId :: CreateConnection -> Lude.Maybe Lude.Text) (\s a -> s {lagId = a} :: CreateConnection)
{-# DEPRECATED ccLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLocation :: Lens.Lens' CreateConnection Lude.Text
ccLocation = Lens.lens (location :: CreateConnection -> Lude.Text) (\s a -> s {location = a} :: CreateConnection)
{-# DEPRECATED ccLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the connection.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConnectionName :: Lens.Lens' CreateConnection Lude.Text
ccConnectionName = Lens.lens (connectionName :: CreateConnection -> Lude.Text) (\s a -> s {connectionName = a} :: CreateConnection)
{-# DEPRECATED ccConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | The bandwidth of the connection.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBandwidth :: Lens.Lens' CreateConnection Lude.Text
ccBandwidth = Lens.lens (bandwidth :: CreateConnection -> Lude.Text) (\s a -> s {bandwidth = a} :: CreateConnection)
{-# DEPRECATED ccBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | The name of the service provider associated with the requested connection.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccProviderName :: Lens.Lens' CreateConnection (Lude.Maybe Lude.Text)
ccProviderName = Lens.lens (providerName :: CreateConnection -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: CreateConnection)
{-# DEPRECATED ccProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The tags to associate with the lag.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateConnection (Lude.Maybe (Lude.NonEmpty Tag))
ccTags = Lens.lens (tags :: CreateConnection -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateConnection)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateConnection where
  type Rs CreateConnection = Connection
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.CreateConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lagId" Lude..=) Lude.<$> lagId,
            Lude.Just ("location" Lude..= location),
            Lude.Just ("connectionName" Lude..= connectionName),
            Lude.Just ("bandwidth" Lude..= bandwidth),
            ("providerName" Lude..=) Lude.<$> providerName,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConnection where
  toQuery = Lude.const Lude.mempty
