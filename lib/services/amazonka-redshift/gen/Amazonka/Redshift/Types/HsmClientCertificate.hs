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
-- Module      : Amazonka.Redshift.Types.HsmClientCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.HsmClientCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
--
-- /See:/ 'newHsmClientCertificate' smart constructor.
data HsmClientCertificate = HsmClientCertificate'
  { -- | The identifier of the HSM client certificate.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The public key that the Amazon Redshift cluster will use to connect to
    -- the HSM. You must register the public key in the HSM.
    hsmClientCertificatePublicKey :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the HSM client certificate.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HsmClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificateIdentifier', 'hsmClientCertificate_hsmClientCertificateIdentifier' - The identifier of the HSM client certificate.
--
-- 'hsmClientCertificatePublicKey', 'hsmClientCertificate_hsmClientCertificatePublicKey' - The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
--
-- 'tags', 'hsmClientCertificate_tags' - The list of tags for the HSM client certificate.
newHsmClientCertificate ::
  HsmClientCertificate
newHsmClientCertificate =
  HsmClientCertificate'
    { hsmClientCertificateIdentifier =
        Prelude.Nothing,
      hsmClientCertificatePublicKey = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The identifier of the HSM client certificate.
hsmClientCertificate_hsmClientCertificateIdentifier :: Lens.Lens' HsmClientCertificate (Prelude.Maybe Prelude.Text)
hsmClientCertificate_hsmClientCertificateIdentifier = Lens.lens (\HsmClientCertificate' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@HsmClientCertificate' {} a -> s {hsmClientCertificateIdentifier = a} :: HsmClientCertificate)

-- | The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
hsmClientCertificate_hsmClientCertificatePublicKey :: Lens.Lens' HsmClientCertificate (Prelude.Maybe Prelude.Text)
hsmClientCertificate_hsmClientCertificatePublicKey = Lens.lens (\HsmClientCertificate' {hsmClientCertificatePublicKey} -> hsmClientCertificatePublicKey) (\s@HsmClientCertificate' {} a -> s {hsmClientCertificatePublicKey = a} :: HsmClientCertificate)

-- | The list of tags for the HSM client certificate.
hsmClientCertificate_tags :: Lens.Lens' HsmClientCertificate (Prelude.Maybe [Tag])
hsmClientCertificate_tags = Lens.lens (\HsmClientCertificate' {tags} -> tags) (\s@HsmClientCertificate' {} a -> s {tags = a} :: HsmClientCertificate) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML HsmClientCertificate where
  parseXML x =
    HsmClientCertificate'
      Prelude.<$> (x Data..@? "HsmClientCertificateIdentifier")
      Prelude.<*> (x Data..@? "HsmClientCertificatePublicKey")
      Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )

instance Prelude.Hashable HsmClientCertificate where
  hashWithSalt _salt HsmClientCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` hsmClientCertificatePublicKey
      `Prelude.hashWithSalt` tags

instance Prelude.NFData HsmClientCertificate where
  rnf HsmClientCertificate' {..} =
    Prelude.rnf hsmClientCertificateIdentifier `Prelude.seq`
      Prelude.rnf hsmClientCertificatePublicKey `Prelude.seq`
        Prelude.rnf tags
