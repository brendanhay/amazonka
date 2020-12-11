{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeletePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a public key you previously added to CloudFront.
module Network.AWS.CloudFront.DeletePublicKey
  ( -- * Creating a request
    DeletePublicKey (..),
    mkDeletePublicKey,

    -- ** Request lenses
    dpkIfMatch,
    dpkId,

    -- * Destructuring the response
    DeletePublicKeyResponse (..),
    mkDeletePublicKeyResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePublicKey' smart constructor.
data DeletePublicKey = DeletePublicKey'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePublicKey' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the public key you want to remove from CloudFront.
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the public key identity to delete. For example: @E2QWRUHAPOMQZL@ .
mkDeletePublicKey ::
  -- | 'id'
  Lude.Text ->
  DeletePublicKey
mkDeletePublicKey pId_ =
  DeletePublicKey' {ifMatch = Lude.Nothing, id = pId_}

-- | The value of the @ETag@ header that you received when retrieving the public key identity to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpkIfMatch :: Lens.Lens' DeletePublicKey (Lude.Maybe Lude.Text)
dpkIfMatch = Lens.lens (ifMatch :: DeletePublicKey -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeletePublicKey)
{-# DEPRECATED dpkIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The ID of the public key you want to remove from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpkId :: Lens.Lens' DeletePublicKey Lude.Text
dpkId = Lens.lens (id :: DeletePublicKey -> Lude.Text) (\s a -> s {id = a} :: DeletePublicKey)
{-# DEPRECATED dpkId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeletePublicKey where
  type Rs DeletePublicKey = DeletePublicKeyResponse
  request = Req.delete cloudFrontService
  response = Res.receiveNull DeletePublicKeyResponse'

instance Lude.ToHeaders DeletePublicKey where
  toHeaders DeletePublicKey' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeletePublicKey where
  toPath DeletePublicKey' {..} =
    Lude.mconcat ["/2020-05-31/public-key/", Lude.toBS id]

instance Lude.ToQuery DeletePublicKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePublicKeyResponse' smart constructor.
data DeletePublicKeyResponse = DeletePublicKeyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePublicKeyResponse' with the minimum fields required to make a request.
mkDeletePublicKeyResponse ::
  DeletePublicKeyResponse
mkDeletePublicKeyResponse = DeletePublicKeyResponse'
