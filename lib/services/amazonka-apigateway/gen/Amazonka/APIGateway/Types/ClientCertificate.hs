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
-- Module      : Amazonka.APIGateway.Types.ClientCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.ClientCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a client certificate used to configure client-side SSL
-- authentication while sending requests to the integration endpoint.
--
-- /See:/ 'newClientCertificate' smart constructor.
data ClientCertificate = ClientCertificate'
  { -- | The identifier of the client certificate.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the client certificate was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the client certificate.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the client certificate will expire.
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | The PEM-encoded public key of the client certificate, which can be used
    -- to configure certificate authentication in the integration endpoint .
    pemEncodedCertificate :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'clientCertificateId', 'clientCertificate_clientCertificateId' - The identifier of the client certificate.
--
-- 'createdDate', 'clientCertificate_createdDate' - The timestamp when the client certificate was created.
--
-- 'description', 'clientCertificate_description' - The description of the client certificate.
--
-- 'expirationDate', 'clientCertificate_expirationDate' - The timestamp when the client certificate will expire.
--
-- 'pemEncodedCertificate', 'clientCertificate_pemEncodedCertificate' - The PEM-encoded public key of the client certificate, which can be used
-- to configure certificate authentication in the integration endpoint .
--
-- 'tags', 'clientCertificate_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
newClientCertificate ::
  ClientCertificate
newClientCertificate =
  ClientCertificate'
    { clientCertificateId =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      pemEncodedCertificate = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The identifier of the client certificate.
clientCertificate_clientCertificateId :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.Text)
clientCertificate_clientCertificateId = Lens.lens (\ClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@ClientCertificate' {} a -> s {clientCertificateId = a} :: ClientCertificate)

-- | The timestamp when the client certificate was created.
clientCertificate_createdDate :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.UTCTime)
clientCertificate_createdDate = Lens.lens (\ClientCertificate' {createdDate} -> createdDate) (\s@ClientCertificate' {} a -> s {createdDate = a} :: ClientCertificate) Prelude.. Lens.mapping Data._Time

-- | The description of the client certificate.
clientCertificate_description :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.Text)
clientCertificate_description = Lens.lens (\ClientCertificate' {description} -> description) (\s@ClientCertificate' {} a -> s {description = a} :: ClientCertificate)

-- | The timestamp when the client certificate will expire.
clientCertificate_expirationDate :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.UTCTime)
clientCertificate_expirationDate = Lens.lens (\ClientCertificate' {expirationDate} -> expirationDate) (\s@ClientCertificate' {} a -> s {expirationDate = a} :: ClientCertificate) Prelude.. Lens.mapping Data._Time

-- | The PEM-encoded public key of the client certificate, which can be used
-- to configure certificate authentication in the integration endpoint .
clientCertificate_pemEncodedCertificate :: Lens.Lens' ClientCertificate (Prelude.Maybe Prelude.Text)
clientCertificate_pemEncodedCertificate = Lens.lens (\ClientCertificate' {pemEncodedCertificate} -> pemEncodedCertificate) (\s@ClientCertificate' {} a -> s {pemEncodedCertificate = a} :: ClientCertificate)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
clientCertificate_tags :: Lens.Lens' ClientCertificate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
clientCertificate_tags = Lens.lens (\ClientCertificate' {tags} -> tags) (\s@ClientCertificate' {} a -> s {tags = a} :: ClientCertificate) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ClientCertificate where
  parseJSON =
    Data.withObject
      "ClientCertificate"
      ( \x ->
          ClientCertificate'
            Prelude.<$> (x Data..:? "clientCertificateId")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "expirationDate")
            Prelude.<*> (x Data..:? "pemEncodedCertificate")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ClientCertificate where
  hashWithSalt _salt ClientCertificate' {..} =
    _salt `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` pemEncodedCertificate
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ClientCertificate where
  rnf ClientCertificate' {..} =
    Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf pemEncodedCertificate
      `Prelude.seq` Prelude.rnf tags
