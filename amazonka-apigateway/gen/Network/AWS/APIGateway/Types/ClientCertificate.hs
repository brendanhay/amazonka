{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ClientCertificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a client certificate used to configure client-side SSL
-- authentication while sending requests to the integration endpoint.
--
-- Client certificates are used to authenticate an API by the backend
-- server. To authenticate an API client (or user), use IAM roles and
-- policies, a custom Authorizer or an Amazon Cognito user pool.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate>
--
-- /See:/ 'newClientCertificate' smart constructor.
data ClientCertificate = ClientCertificate'
  { -- | The timestamp when the client certificate was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The timestamp when the client certificate will expire.
    expirationDate :: Core.Maybe Core.POSIX,
    -- | The PEM-encoded public key of the client certificate, which can be used
    -- to configure certificate authentication in the integration endpoint .
    pemEncodedCertificate :: Core.Maybe Core.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The identifier of the client certificate.
    clientCertificateId :: Core.Maybe Core.Text,
    -- | The description of the client certificate.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'clientCertificate_createdDate' - The timestamp when the client certificate was created.
--
-- 'expirationDate', 'clientCertificate_expirationDate' - The timestamp when the client certificate will expire.
--
-- 'pemEncodedCertificate', 'clientCertificate_pemEncodedCertificate' - The PEM-encoded public key of the client certificate, which can be used
-- to configure certificate authentication in the integration endpoint .
--
-- 'tags', 'clientCertificate_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'clientCertificateId', 'clientCertificate_clientCertificateId' - The identifier of the client certificate.
--
-- 'description', 'clientCertificate_description' - The description of the client certificate.
newClientCertificate ::
  ClientCertificate
newClientCertificate =
  ClientCertificate'
    { createdDate = Core.Nothing,
      expirationDate = Core.Nothing,
      pemEncodedCertificate = Core.Nothing,
      tags = Core.Nothing,
      clientCertificateId = Core.Nothing,
      description = Core.Nothing
    }

-- | The timestamp when the client certificate was created.
clientCertificate_createdDate :: Lens.Lens' ClientCertificate (Core.Maybe Core.UTCTime)
clientCertificate_createdDate = Lens.lens (\ClientCertificate' {createdDate} -> createdDate) (\s@ClientCertificate' {} a -> s {createdDate = a} :: ClientCertificate) Core.. Lens.mapping Core._Time

-- | The timestamp when the client certificate will expire.
clientCertificate_expirationDate :: Lens.Lens' ClientCertificate (Core.Maybe Core.UTCTime)
clientCertificate_expirationDate = Lens.lens (\ClientCertificate' {expirationDate} -> expirationDate) (\s@ClientCertificate' {} a -> s {expirationDate = a} :: ClientCertificate) Core.. Lens.mapping Core._Time

-- | The PEM-encoded public key of the client certificate, which can be used
-- to configure certificate authentication in the integration endpoint .
clientCertificate_pemEncodedCertificate :: Lens.Lens' ClientCertificate (Core.Maybe Core.Text)
clientCertificate_pemEncodedCertificate = Lens.lens (\ClientCertificate' {pemEncodedCertificate} -> pemEncodedCertificate) (\s@ClientCertificate' {} a -> s {pemEncodedCertificate = a} :: ClientCertificate)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
clientCertificate_tags :: Lens.Lens' ClientCertificate (Core.Maybe (Core.HashMap Core.Text Core.Text))
clientCertificate_tags = Lens.lens (\ClientCertificate' {tags} -> tags) (\s@ClientCertificate' {} a -> s {tags = a} :: ClientCertificate) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the client certificate.
clientCertificate_clientCertificateId :: Lens.Lens' ClientCertificate (Core.Maybe Core.Text)
clientCertificate_clientCertificateId = Lens.lens (\ClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@ClientCertificate' {} a -> s {clientCertificateId = a} :: ClientCertificate)

-- | The description of the client certificate.
clientCertificate_description :: Lens.Lens' ClientCertificate (Core.Maybe Core.Text)
clientCertificate_description = Lens.lens (\ClientCertificate' {description} -> description) (\s@ClientCertificate' {} a -> s {description = a} :: ClientCertificate)

instance Core.FromJSON ClientCertificate where
  parseJSON =
    Core.withObject
      "ClientCertificate"
      ( \x ->
          ClientCertificate'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "expirationDate")
            Core.<*> (x Core..:? "pemEncodedCertificate")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "clientCertificateId")
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable ClientCertificate

instance Core.NFData ClientCertificate
