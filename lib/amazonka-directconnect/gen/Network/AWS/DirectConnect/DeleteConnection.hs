{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection.
--
-- Deleting a connection only stops the AWS Direct Connect port hour and data transfer charges. If you are partnering with any third parties to connect with the AWS Direct Connect location, you must cancel your service with them separately.
module Network.AWS.DirectConnect.DeleteConnection
  ( -- * Creating a request
    DeleteConnection (..),
    mkDeleteConnection,

    -- ** Request lenses
    dcConnectionId,

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

-- | /See:/ 'mkDeleteConnection' smart constructor.
newtype DeleteConnection = DeleteConnection'
  { -- | The ID of the connection.
    connectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
mkDeleteConnection ::
  -- | 'connectionId'
  Lude.Text ->
  DeleteConnection
mkDeleteConnection pConnectionId_ =
  DeleteConnection' {connectionId = pConnectionId_}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectionId :: Lens.Lens' DeleteConnection Lude.Text
dcConnectionId = Lens.lens (connectionId :: DeleteConnection -> Lude.Text) (\s a -> s {connectionId = a} :: DeleteConnection)
{-# DEPRECATED dcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest DeleteConnection where
  type Rs DeleteConnection = Connection
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DeleteConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DeleteConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("connectionId" Lude..= connectionId)])

instance Lude.ToPath DeleteConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConnection where
  toQuery = Lude.const Lude.mempty
