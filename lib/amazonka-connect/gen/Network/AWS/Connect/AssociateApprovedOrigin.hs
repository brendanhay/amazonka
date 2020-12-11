{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateApprovedOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an approved origin to an Amazon Connect instance.
module Network.AWS.Connect.AssociateApprovedOrigin
  ( -- * Creating a request
    AssociateApprovedOrigin (..),
    mkAssociateApprovedOrigin,

    -- ** Request lenses
    aaoInstanceId,
    aaoOrigin,

    -- * Destructuring the response
    AssociateApprovedOriginResponse (..),
    mkAssociateApprovedOriginResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateApprovedOrigin' smart constructor.
data AssociateApprovedOrigin = AssociateApprovedOrigin'
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

-- | Creates a value of 'AssociateApprovedOrigin' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'origin' - The domain to add to your allow list.
mkAssociateApprovedOrigin ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'origin'
  Lude.Text ->
  AssociateApprovedOrigin
mkAssociateApprovedOrigin pInstanceId_ pOrigin_ =
  AssociateApprovedOrigin'
    { instanceId = pInstanceId_,
      origin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoInstanceId :: Lens.Lens' AssociateApprovedOrigin Lude.Text
aaoInstanceId = Lens.lens (instanceId :: AssociateApprovedOrigin -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateApprovedOrigin)
{-# DEPRECATED aaoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The domain to add to your allow list.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoOrigin :: Lens.Lens' AssociateApprovedOrigin Lude.Text
aaoOrigin = Lens.lens (origin :: AssociateApprovedOrigin -> Lude.Text) (\s a -> s {origin = a} :: AssociateApprovedOrigin)
{-# DEPRECATED aaoOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

instance Lude.AWSRequest AssociateApprovedOrigin where
  type Rs AssociateApprovedOrigin = AssociateApprovedOriginResponse
  request = Req.putJSON connectService
  response = Res.receiveNull AssociateApprovedOriginResponse'

instance Lude.ToHeaders AssociateApprovedOrigin where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateApprovedOrigin where
  toJSON AssociateApprovedOrigin' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Origin" Lude..= origin)])

instance Lude.ToPath AssociateApprovedOrigin where
  toPath AssociateApprovedOrigin' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/approved-origin"]

instance Lude.ToQuery AssociateApprovedOrigin where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateApprovedOriginResponse' smart constructor.
data AssociateApprovedOriginResponse = AssociateApprovedOriginResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateApprovedOriginResponse' with the minimum fields required to make a request.
mkAssociateApprovedOriginResponse ::
  AssociateApprovedOriginResponse
mkAssociateApprovedOriginResponse =
  AssociateApprovedOriginResponse'
