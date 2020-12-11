{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartAssociationsOnce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to run an association immediately and only one time. This action can be helpful when troubleshooting associations.
module Network.AWS.SSM.StartAssociationsOnce
  ( -- * Creating a request
    StartAssociationsOnce (..),
    mkStartAssociationsOnce,

    -- ** Request lenses
    saoAssociationIds,

    -- * Destructuring the response
    StartAssociationsOnceResponse (..),
    mkStartAssociationsOnceResponse,

    -- ** Response lenses
    saorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkStartAssociationsOnce' smart constructor.
newtype StartAssociationsOnce = StartAssociationsOnce'
  { associationIds ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAssociationsOnce' with the minimum fields required to make a request.
--
-- * 'associationIds' - The association IDs that you want to run immediately and only one time.
mkStartAssociationsOnce ::
  -- | 'associationIds'
  Lude.NonEmpty Lude.Text ->
  StartAssociationsOnce
mkStartAssociationsOnce pAssociationIds_ =
  StartAssociationsOnce' {associationIds = pAssociationIds_}

-- | The association IDs that you want to run immediately and only one time.
--
-- /Note:/ Consider using 'associationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saoAssociationIds :: Lens.Lens' StartAssociationsOnce (Lude.NonEmpty Lude.Text)
saoAssociationIds = Lens.lens (associationIds :: StartAssociationsOnce -> Lude.NonEmpty Lude.Text) (\s a -> s {associationIds = a} :: StartAssociationsOnce)
{-# DEPRECATED saoAssociationIds "Use generic-lens or generic-optics with 'associationIds' instead." #-}

instance Lude.AWSRequest StartAssociationsOnce where
  type Rs StartAssociationsOnce = StartAssociationsOnceResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartAssociationsOnceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAssociationsOnce where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.StartAssociationsOnce" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartAssociationsOnce where
  toJSON StartAssociationsOnce' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AssociationIds" Lude..= associationIds)]
      )

instance Lude.ToPath StartAssociationsOnce where
  toPath = Lude.const "/"

instance Lude.ToQuery StartAssociationsOnce where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAssociationsOnceResponse' smart constructor.
newtype StartAssociationsOnceResponse = StartAssociationsOnceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAssociationsOnceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartAssociationsOnceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartAssociationsOnceResponse
mkStartAssociationsOnceResponse pResponseStatus_ =
  StartAssociationsOnceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saorsResponseStatus :: Lens.Lens' StartAssociationsOnceResponse Lude.Int
saorsResponseStatus = Lens.lens (responseStatus :: StartAssociationsOnceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAssociationsOnceResponse)
{-# DEPRECATED saorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
