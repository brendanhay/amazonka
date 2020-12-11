{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'ClientCertificate' resource.
module Network.AWS.APIGateway.DeleteClientCertificate
  ( -- * Creating a request
    DeleteClientCertificate (..),
    mkDeleteClientCertificate,

    -- ** Request lenses
    dccClientCertificateId,

    -- * Destructuring the response
    DeleteClientCertificateResponse (..),
    mkDeleteClientCertificateResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete the 'ClientCertificate' resource.
--
-- /See:/ 'mkDeleteClientCertificate' smart constructor.
newtype DeleteClientCertificate = DeleteClientCertificate'
  { clientCertificateId ::
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

-- | Creates a value of 'DeleteClientCertificate' with the minimum fields required to make a request.
--
-- * 'clientCertificateId' - [Required] The identifier of the 'ClientCertificate' resource to be deleted.
mkDeleteClientCertificate ::
  -- | 'clientCertificateId'
  Lude.Text ->
  DeleteClientCertificate
mkDeleteClientCertificate pClientCertificateId_ =
  DeleteClientCertificate'
    { clientCertificateId =
        pClientCertificateId_
    }

-- | [Required] The identifier of the 'ClientCertificate' resource to be deleted.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccClientCertificateId :: Lens.Lens' DeleteClientCertificate Lude.Text
dccClientCertificateId = Lens.lens (clientCertificateId :: DeleteClientCertificate -> Lude.Text) (\s a -> s {clientCertificateId = a} :: DeleteClientCertificate)
{-# DEPRECATED dccClientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead." #-}

instance Lude.AWSRequest DeleteClientCertificate where
  type Rs DeleteClientCertificate = DeleteClientCertificateResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteClientCertificateResponse'

instance Lude.ToHeaders DeleteClientCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteClientCertificate where
  toPath DeleteClientCertificate' {..} =
    Lude.mconcat
      ["/clientcertificates/", Lude.toBS clientCertificateId]

instance Lude.ToQuery DeleteClientCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteClientCertificateResponse' smart constructor.
data DeleteClientCertificateResponse = DeleteClientCertificateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClientCertificateResponse' with the minimum fields required to make a request.
mkDeleteClientCertificateResponse ::
  DeleteClientCertificateResponse
mkDeleteClientCertificateResponse =
  DeleteClientCertificateResponse'
