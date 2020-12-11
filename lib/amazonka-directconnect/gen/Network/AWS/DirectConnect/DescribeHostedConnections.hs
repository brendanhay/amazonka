{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeHostedConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosted connections that have been provisioned on the specified interconnect or link aggregation group (LAG).
module Network.AWS.DirectConnect.DescribeHostedConnections
  ( -- * Creating a request
    DescribeHostedConnections (..),
    mkDescribeHostedConnections,

    -- ** Request lenses
    dhcConnectionId,

    -- * Destructuring the response
    Connections (..),
    mkConnections,

    -- ** Response lenses
    cConnections,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHostedConnections' smart constructor.
newtype DescribeHostedConnections = DescribeHostedConnections'
  { connectionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHostedConnections' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the interconnect or LAG.
mkDescribeHostedConnections ::
  -- | 'connectionId'
  Lude.Text ->
  DescribeHostedConnections
mkDescribeHostedConnections pConnectionId_ =
  DescribeHostedConnections' {connectionId = pConnectionId_}

-- | The ID of the interconnect or LAG.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcConnectionId :: Lens.Lens' DescribeHostedConnections Lude.Text
dhcConnectionId = Lens.lens (connectionId :: DescribeHostedConnections -> Lude.Text) (\s a -> s {connectionId = a} :: DescribeHostedConnections)
{-# DEPRECATED dhcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest DescribeHostedConnections where
  type Rs DescribeHostedConnections = Connections
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeHostedConnections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeHostedConnections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHostedConnections where
  toJSON DescribeHostedConnections' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("connectionId" Lude..= connectionId)])

instance Lude.ToPath DescribeHostedConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHostedConnections where
  toQuery = Lude.const Lude.mempty
