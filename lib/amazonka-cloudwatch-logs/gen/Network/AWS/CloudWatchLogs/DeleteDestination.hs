{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified destination, and eventually disables all the subscription filters that publish to it. This operation does not delete the physical resource encapsulated by the destination.
module Network.AWS.CloudWatchLogs.DeleteDestination
  ( -- * Creating a request
    DeleteDestination (..),
    mkDeleteDestination,

    -- ** Request lenses
    ddDestinationName,

    -- * Destructuring the response
    DeleteDestinationResponse (..),
    mkDeleteDestinationResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDestination' smart constructor.
newtype DeleteDestination = DeleteDestination'
  { destinationName ::
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

-- | Creates a value of 'DeleteDestination' with the minimum fields required to make a request.
--
-- * 'destinationName' - The name of the destination.
mkDeleteDestination ::
  -- | 'destinationName'
  Lude.Text ->
  DeleteDestination
mkDeleteDestination pDestinationName_ =
  DeleteDestination' {destinationName = pDestinationName_}

-- | The name of the destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDestinationName :: Lens.Lens' DeleteDestination Lude.Text
ddDestinationName = Lens.lens (destinationName :: DeleteDestination -> Lude.Text) (\s a -> s {destinationName = a} :: DeleteDestination)
{-# DEPRECATED ddDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

instance Lude.AWSRequest DeleteDestination where
  type Rs DeleteDestination = DeleteDestinationResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteDestinationResponse'

instance Lude.ToHeaders DeleteDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteDestination" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDestination where
  toJSON DeleteDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("destinationName" Lude..= destinationName)]
      )

instance Lude.ToPath DeleteDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDestinationResponse' smart constructor.
data DeleteDestinationResponse = DeleteDestinationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDestinationResponse' with the minimum fields required to make a request.
mkDeleteDestinationResponse ::
  DeleteDestinationResponse
mkDeleteDestinationResponse = DeleteDestinationResponse'
