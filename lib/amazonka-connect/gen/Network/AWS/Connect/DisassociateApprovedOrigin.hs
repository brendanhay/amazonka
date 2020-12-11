{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateApprovedOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes access to integrated applications from Amazon Connect.
module Network.AWS.Connect.DisassociateApprovedOrigin
  ( -- * Creating a request
    DisassociateApprovedOrigin (..),
    mkDisassociateApprovedOrigin,

    -- ** Request lenses
    daoInstanceId,
    daoOrigin,

    -- * Destructuring the response
    DisassociateApprovedOriginResponse (..),
    mkDisassociateApprovedOriginResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateApprovedOrigin' smart constructor.
data DisassociateApprovedOrigin = DisassociateApprovedOrigin'
  { instanceId ::
      Lude.Text,
    origin :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateApprovedOrigin' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'origin' - The domain URL of the integrated application.
mkDisassociateApprovedOrigin ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'origin'
  Lude.Text ->
  DisassociateApprovedOrigin
mkDisassociateApprovedOrigin pInstanceId_ pOrigin_ =
  DisassociateApprovedOrigin'
    { instanceId = pInstanceId_,
      origin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoInstanceId :: Lens.Lens' DisassociateApprovedOrigin Lude.Text
daoInstanceId = Lens.lens (instanceId :: DisassociateApprovedOrigin -> Lude.Text) (\s a -> s {instanceId = a} :: DisassociateApprovedOrigin)
{-# DEPRECATED daoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The domain URL of the integrated application.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoOrigin :: Lens.Lens' DisassociateApprovedOrigin Lude.Text
daoOrigin = Lens.lens (origin :: DisassociateApprovedOrigin -> Lude.Text) (\s a -> s {origin = a} :: DisassociateApprovedOrigin)
{-# DEPRECATED daoOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

instance Lude.AWSRequest DisassociateApprovedOrigin where
  type
    Rs DisassociateApprovedOrigin =
      DisassociateApprovedOriginResponse
  request = Req.delete connectService
  response = Res.receiveNull DisassociateApprovedOriginResponse'

instance Lude.ToHeaders DisassociateApprovedOrigin where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateApprovedOrigin where
  toPath DisassociateApprovedOrigin' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/approved-origin"]

instance Lude.ToQuery DisassociateApprovedOrigin where
  toQuery DisassociateApprovedOrigin' {..} =
    Lude.mconcat ["origin" Lude.=: origin]

-- | /See:/ 'mkDisassociateApprovedOriginResponse' smart constructor.
data DisassociateApprovedOriginResponse = DisassociateApprovedOriginResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateApprovedOriginResponse' with the minimum fields required to make a request.
mkDisassociateApprovedOriginResponse ::
  DisassociateApprovedOriginResponse
mkDisassociateApprovedOriginResponse =
  DisassociateApprovedOriginResponse'
