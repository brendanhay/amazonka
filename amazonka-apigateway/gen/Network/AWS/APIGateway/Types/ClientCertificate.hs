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
import qualified Network.AWS.Prelude as Prelude

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
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The PEM-encoded public key of the client certificate, which can be used
    -- to configure certificate authentication in the integration endpoint .
    pemEncodedCertificate :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the client certificate will expire.
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the client certificate.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the client certificate.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'pemEncodedCertificate', 'clientCertificate_pemEncodedCertificate' - The PEM-encoded public key of the client certificate, which can be used
-- to configure certificate authentication in the integration endpoint .
--
-- 'expirationDate', 'clientCertificate_expirationDate' - The timestamp when the client certificate will expire.
--
-- 'clientCertificateId', 'clientCertificate_clientCertificateId' - The identifier of the client certificate.
--
-- 'tags', 'clientCertificate_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'description', 'clientCertificate_description' - The description of the client certificate.
newClientCertificate ::
  ClientCertificate
newClientCertificate =
  ClientCertificate'
    { createdDate = Prelude.Nothing,
      pemEncodedCertificate = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The timestamp when the client certificate was created.
clientCertificate_createdDate :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.UTCTime)
clientCertificate_createdDate = Lens.lens (\ClientCertificate' {createdDate} -> createdDate) (\s@ClientCertificate' {} a -> s {createdDate = a} :: ClientCertificate) Prelude.. Lens.mapping Core._Time

-- | The PEM-encoded public key of the client certificate, which can be used
-- to configure certificate authentication in the integration endpoint .
clientCertificate_pemEncodedCertificate :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.Text)
clientCertificate_pemEncodedCertificate = Lens.lens (\ClientCertificate' {pemEncodedCertificate} -> pemEncodedCertificate) (\s@ClientCertificate' {} a -> s {pemEncodedCertificate = a} :: ClientCertificate)

-- | The timestamp when the client certificate will expire.
clientCertificate_expirationDate :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.UTCTime)
clientCertificate_expirationDate = Lens.lens (\ClientCertificate' {expirationDate} -> expirationDate) (\s@ClientCertificate' {} a -> s {expirationDate = a} :: ClientCertificate) Prelude.. Lens.mapping Core._Time

-- | The identifier of the client certificate.
clientCertificate_clientCertificateId :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.Text)
clientCertificate_clientCertificateId = Lens.lens (\ClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@ClientCertificate' {} a -> s {clientCertificateId = a} :: ClientCertificate)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
clientCertificate_tags :: Lens.Lens' ClientCertificate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
clientCertificate_tags = Lens.lens (\ClientCertificate' {tags} -> tags) (\s@ClientCertificate' {} a -> s {tags = a} :: ClientCertificate) Prelude.. Lens.mapping Lens._Coerce

-- | The description of the client certificate.
clientCertificate_description :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.Text)
clientCertificate_description = Lens.lens (\ClientCertificate' {description} -> description) (\s@ClientCertificate' {} a -> s {description = a} :: ClientCertificate)

instance Core.FromJSON ClientCertificate where
  parseJSON =
    Core.withObject
      "ClientCertificate"
      ( \x ->
          ClientCertificate'
            Prelude.<$> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "pemEncodedCertificate")
            Prelude.<*> (x Core..:? "expirationDate")
            Prelude.<*> (x Core..:? "clientCertificateId")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable ClientCertificate

instance Prelude.NFData ClientCertificate
