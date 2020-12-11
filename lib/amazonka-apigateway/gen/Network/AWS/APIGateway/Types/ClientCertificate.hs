-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ClientCertificate
  ( ClientCertificate (..),

    -- * Smart constructor
    mkClientCertificate,

    -- * Lenses
    ccPemEncodedCertificate,
    ccClientCertificateId,
    ccCreatedDate,
    ccExpirationDate,
    ccDescription,
    ccTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a client certificate used to configure client-side SSL authentication while sending requests to the integration endpoint.
--
-- Client certificates are used to authenticate an API by the backend server. To authenticate an API client (or user), use IAM roles and policies, a custom 'Authorizer' or an Amazon Cognito user pool.<https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate>
--
-- /See:/ 'mkClientCertificate' smart constructor.
data ClientCertificate = ClientCertificate'
  { pemEncodedCertificate ::
      Lude.Maybe Lude.Text,
    clientCertificateId :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    expirationDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientCertificate' with the minimum fields required to make a request.
--
-- * 'clientCertificateId' - The identifier of the client certificate.
-- * 'createdDate' - The timestamp when the client certificate was created.
-- * 'description' - The description of the client certificate.
-- * 'expirationDate' - The timestamp when the client certificate will expire.
-- * 'pemEncodedCertificate' - The PEM-encoded public key of the client certificate, which can be used to configure certificate authentication in the integration endpoint .
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
mkClientCertificate ::
  ClientCertificate
mkClientCertificate =
  ClientCertificate'
    { pemEncodedCertificate = Lude.Nothing,
      clientCertificateId = Lude.Nothing,
      createdDate = Lude.Nothing,
      expirationDate = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The PEM-encoded public key of the client certificate, which can be used to configure certificate authentication in the integration endpoint .
--
-- /Note:/ Consider using 'pemEncodedCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPemEncodedCertificate :: Lens.Lens' ClientCertificate (Lude.Maybe Lude.Text)
ccPemEncodedCertificate = Lens.lens (pemEncodedCertificate :: ClientCertificate -> Lude.Maybe Lude.Text) (\s a -> s {pemEncodedCertificate = a} :: ClientCertificate)
{-# DEPRECATED ccPemEncodedCertificate "Use generic-lens or generic-optics with 'pemEncodedCertificate' instead." #-}

-- | The identifier of the client certificate.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientCertificateId :: Lens.Lens' ClientCertificate (Lude.Maybe Lude.Text)
ccClientCertificateId = Lens.lens (clientCertificateId :: ClientCertificate -> Lude.Maybe Lude.Text) (\s a -> s {clientCertificateId = a} :: ClientCertificate)
{-# DEPRECATED ccClientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead." #-}

-- | The timestamp when the client certificate was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCreatedDate :: Lens.Lens' ClientCertificate (Lude.Maybe Lude.Timestamp)
ccCreatedDate = Lens.lens (createdDate :: ClientCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: ClientCertificate)
{-# DEPRECATED ccCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The timestamp when the client certificate will expire.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpirationDate :: Lens.Lens' ClientCertificate (Lude.Maybe Lude.Timestamp)
ccExpirationDate = Lens.lens (expirationDate :: ClientCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: ClientCertificate)
{-# DEPRECATED ccExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The description of the client certificate.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' ClientCertificate (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: ClientCertificate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClientCertificate)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' ClientCertificate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccTags = Lens.lens (tags :: ClientCertificate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ClientCertificate)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ClientCertificate where
  parseJSON =
    Lude.withObject
      "ClientCertificate"
      ( \x ->
          ClientCertificate'
            Lude.<$> (x Lude..:? "pemEncodedCertificate")
            Lude.<*> (x Lude..:? "clientCertificateId")
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "expirationDate")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
