{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the specified connection or all connections in this Region.
module Network.AWS.DirectConnect.DescribeConnections
  ( -- * Creating a request
    DescribeConnections (..),
    mkDescribeConnections,

    -- ** Request lenses
    dConnectionId,

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

-- | /See:/ 'mkDescribeConnections' smart constructor.
newtype DescribeConnections = DescribeConnections'
  { connectionId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConnections' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
mkDescribeConnections ::
  DescribeConnections
mkDescribeConnections =
  DescribeConnections' {connectionId = Lude.Nothing}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConnectionId :: Lens.Lens' DescribeConnections (Lude.Maybe Lude.Text)
dConnectionId = Lens.lens (connectionId :: DescribeConnections -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: DescribeConnections)
{-# DEPRECATED dConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest DescribeConnections where
  type Rs DescribeConnections = Connections
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeConnections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeConnections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConnections where
  toJSON DescribeConnections' {..} =
    Lude.object
      (Lude.catMaybes [("connectionId" Lude..=) Lude.<$> connectionId])

instance Lude.ToPath DescribeConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConnections where
  toQuery = Lude.const Lude.mempty
