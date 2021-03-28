{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.ClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.ClientCertificate
  ( ClientCertificate (..)
  -- * Smart constructor
  , mkClientCertificate
  -- * Lenses
  , ccClientCertificateId
  , ccCreatedDate
  , ccDescription
  , ccExpirationDate
  , ccPemEncodedCertificate
  , ccTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a client certificate used to configure client-side SSL authentication while sending requests to the integration endpoint.
--
-- Client certificates are used to authenticate an API by the backend server. To authenticate an API client (or user), use IAM roles and policies, a custom 'Authorizer' or an Amazon Cognito user pool.<https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate> 
--
-- /See:/ 'mkClientCertificate' smart constructor.
data ClientCertificate = ClientCertificate'
  { clientCertificateId :: Core.Maybe Core.Text
    -- ^ The identifier of the client certificate.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the client certificate was created.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the client certificate.
  , expirationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the client certificate will expire.
  , pemEncodedCertificate :: Core.Maybe Core.Text
    -- ^ The PEM-encoded public key of the client certificate, which can be used to configure certificate authentication in the integration endpoint .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClientCertificate' value with any optional fields omitted.
mkClientCertificate
    :: ClientCertificate
mkClientCertificate
  = ClientCertificate'{clientCertificateId = Core.Nothing,
                       createdDate = Core.Nothing, description = Core.Nothing,
                       expirationDate = Core.Nothing,
                       pemEncodedCertificate = Core.Nothing, tags = Core.Nothing}

-- | The identifier of the client certificate.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientCertificateId :: Lens.Lens' ClientCertificate (Core.Maybe Core.Text)
ccClientCertificateId = Lens.field @"clientCertificateId"
{-# INLINEABLE ccClientCertificateId #-}
{-# DEPRECATED clientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead"  #-}

-- | The timestamp when the client certificate was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCreatedDate :: Lens.Lens' ClientCertificate (Core.Maybe Core.NominalDiffTime)
ccCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE ccCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The description of the client certificate.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' ClientCertificate (Core.Maybe Core.Text)
ccDescription = Lens.field @"description"
{-# INLINEABLE ccDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The timestamp when the client certificate will expire.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpirationDate :: Lens.Lens' ClientCertificate (Core.Maybe Core.NominalDiffTime)
ccExpirationDate = Lens.field @"expirationDate"
{-# INLINEABLE ccExpirationDate #-}
{-# DEPRECATED expirationDate "Use generic-lens or generic-optics with 'expirationDate' instead"  #-}

-- | The PEM-encoded public key of the client certificate, which can be used to configure certificate authentication in the integration endpoint .
--
-- /Note:/ Consider using 'pemEncodedCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPemEncodedCertificate :: Lens.Lens' ClientCertificate (Core.Maybe Core.Text)
ccPemEncodedCertificate = Lens.field @"pemEncodedCertificate"
{-# INLINEABLE ccPemEncodedCertificate #-}
{-# DEPRECATED pemEncodedCertificate "Use generic-lens or generic-optics with 'pemEncodedCertificate' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' ClientCertificate (Core.Maybe (Core.HashMap Core.Text Core.Text))
ccTags = Lens.field @"tags"
{-# INLINEABLE ccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON ClientCertificate where
        parseJSON
          = Core.withObject "ClientCertificate" Core.$
              \ x ->
                ClientCertificate' Core.<$>
                  (x Core..:? "clientCertificateId") Core.<*>
                    x Core..:? "createdDate"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "expirationDate"
                    Core.<*> x Core..:? "pemEncodedCertificate"
                    Core.<*> x Core..:? "tags"
