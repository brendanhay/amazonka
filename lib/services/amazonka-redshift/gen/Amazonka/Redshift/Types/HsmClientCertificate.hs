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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.HsmClientCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
--
-- /See:/ 'newHsmClientCertificate' smart constructor.
data HsmClientCertificate = HsmClientCertificate'
  { -- | The list of tags for the HSM client certificate.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the HSM client certificate.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The public key that the Amazon Redshift cluster will use to connect to
    -- the HSM. You must register the public key in the HSM.
    hsmClientCertificatePublicKey :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'hsmClientCertificate_tags' - The list of tags for the HSM client certificate.
--
-- 'hsmClientCertificateIdentifier', 'hsmClientCertificate_hsmClientCertificateIdentifier' - The identifier of the HSM client certificate.
--
-- 'hsmClientCertificatePublicKey', 'hsmClientCertificate_hsmClientCertificatePublicKey' - The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
newHsmClientCertificate ::
  HsmClientCertificate
newHsmClientCertificate =
  HsmClientCertificate'
    { tags = Prelude.Nothing,
      hsmClientCertificateIdentifier = Prelude.Nothing,
      hsmClientCertificatePublicKey = Prelude.Nothing
    }

-- | The list of tags for the HSM client certificate.
hsmClientCertificate_tags :: Lens.Lens' HsmClientCertificate (Prelude.Maybe [Tag])
hsmClientCertificate_tags = Lens.lens (\HsmClientCertificate' {tags} -> tags) (\s@HsmClientCertificate' {} a -> s {tags = a} :: HsmClientCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the HSM client certificate.
hsmClientCertificate_hsmClientCertificateIdentifier :: Lens.Lens' HsmClientCertificate (Prelude.Maybe Prelude.Text)
hsmClientCertificate_hsmClientCertificateIdentifier = Lens.lens (\HsmClientCertificate' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@HsmClientCertificate' {} a -> s {hsmClientCertificateIdentifier = a} :: HsmClientCertificate)

-- | The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
hsmClientCertificate_hsmClientCertificatePublicKey :: Lens.Lens' HsmClientCertificate (Prelude.Maybe Prelude.Text)
hsmClientCertificate_hsmClientCertificatePublicKey = Lens.lens (\HsmClientCertificate' {hsmClientCertificatePublicKey} -> hsmClientCertificatePublicKey) (\s@HsmClientCertificate' {} a -> s {hsmClientCertificatePublicKey = a} :: HsmClientCertificate)

instance Core.FromXML HsmClientCertificate where
  parseXML x =
    HsmClientCertificate'
      Prelude.<$> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "HsmClientCertificateIdentifier")
      Prelude.<*> (x Core..@? "HsmClientCertificatePublicKey")

instance Prelude.Hashable HsmClientCertificate where
  hashWithSalt _salt HsmClientCertificate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` hsmClientCertificatePublicKey

instance Prelude.NFData HsmClientCertificate where
  rnf HsmClientCertificate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hsmClientCertificateIdentifier
      `Prelude.seq` Prelude.rnf hsmClientCertificatePublicKey
