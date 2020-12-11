{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an 'ClientCertificate' resource.
module Network.AWS.APIGateway.UpdateClientCertificate
  ( -- * Creating a request
    UpdateClientCertificate (..),
    mkUpdateClientCertificate,

    -- ** Request lenses
    uccPatchOperations,
    uccClientCertificateId,

    -- * Destructuring the response
    ClientCertificate (..),
    mkClientCertificate,

    -- ** Response lenses
    ccPemEncodedCertificate,
    ccClientCertificateId,
    ccCreatedDate,
    ccExpirationDate,
    ccDescription,
    ccTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to change information about an 'ClientCertificate' resource.
--
-- /See:/ 'mkUpdateClientCertificate' smart constructor.
data UpdateClientCertificate = UpdateClientCertificate'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    clientCertificateId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClientCertificate' with the minimum fields required to make a request.
--
-- * 'clientCertificateId' - [Required] The identifier of the 'ClientCertificate' resource to be updated.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateClientCertificate ::
  -- | 'clientCertificateId'
  Lude.Text ->
  UpdateClientCertificate
mkUpdateClientCertificate pClientCertificateId_ =
  UpdateClientCertificate'
    { patchOperations = Lude.Nothing,
      clientCertificateId = pClientCertificateId_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccPatchOperations :: Lens.Lens' UpdateClientCertificate (Lude.Maybe [PatchOperation])
uccPatchOperations = Lens.lens (patchOperations :: UpdateClientCertificate -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateClientCertificate)
{-# DEPRECATED uccPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The identifier of the 'ClientCertificate' resource to be updated.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccClientCertificateId :: Lens.Lens' UpdateClientCertificate Lude.Text
uccClientCertificateId = Lens.lens (clientCertificateId :: UpdateClientCertificate -> Lude.Text) (\s a -> s {clientCertificateId = a} :: UpdateClientCertificate)
{-# DEPRECATED uccClientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead." #-}

instance Lude.AWSRequest UpdateClientCertificate where
  type Rs UpdateClientCertificate = ClientCertificate
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateClientCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateClientCertificate where
  toJSON UpdateClientCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateClientCertificate where
  toPath UpdateClientCertificate' {..} =
    Lude.mconcat
      ["/clientcertificates/", Lude.toBS clientCertificateId]

instance Lude.ToQuery UpdateClientCertificate where
  toQuery = Lude.const Lude.mempty
