{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ClientCertificate' resource.
module Network.AWS.APIGateway.GetClientCertificate
  ( -- * Creating a request
    GetClientCertificate (..),
    mkGetClientCertificate,

    -- ** Request lenses
    gccClientCertificateId,

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

-- | A request to get information about the current 'ClientCertificate' resource.
--
-- /See:/ 'mkGetClientCertificate' smart constructor.
newtype GetClientCertificate = GetClientCertificate'
  { -- | [Required] The identifier of the 'ClientCertificate' resource to be described.
    clientCertificateId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClientCertificate' with the minimum fields required to make a request.
--
-- * 'clientCertificateId' - [Required] The identifier of the 'ClientCertificate' resource to be described.
mkGetClientCertificate ::
  -- | 'clientCertificateId'
  Lude.Text ->
  GetClientCertificate
mkGetClientCertificate pClientCertificateId_ =
  GetClientCertificate'
    { clientCertificateId =
        pClientCertificateId_
    }

-- | [Required] The identifier of the 'ClientCertificate' resource to be described.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccClientCertificateId :: Lens.Lens' GetClientCertificate Lude.Text
gccClientCertificateId = Lens.lens (clientCertificateId :: GetClientCertificate -> Lude.Text) (\s a -> s {clientCertificateId = a} :: GetClientCertificate)
{-# DEPRECATED gccClientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead." #-}

instance Lude.AWSRequest GetClientCertificate where
  type Rs GetClientCertificate = ClientCertificate
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetClientCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetClientCertificate where
  toPath GetClientCertificate' {..} =
    Lude.mconcat
      ["/clientcertificates/", Lude.toBS clientCertificateId]

instance Lude.ToQuery GetClientCertificate where
  toQuery = Lude.const Lude.mempty
