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
-- Module      : Network.AWS.Redshift.Types.HsmClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HsmClientCertificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
--
-- /See:/ 'newHsmClientCertificate' smart constructor.
data HsmClientCertificate = HsmClientCertificate'
  { -- | The public key that the Amazon Redshift cluster will use to connect to
    -- the HSM. You must register the public key in the HSM.
    hsmClientCertificatePublicKey :: Core.Maybe Core.Text,
    -- | The identifier of the HSM client certificate.
    hsmClientCertificateIdentifier :: Core.Maybe Core.Text,
    -- | The list of tags for the HSM client certificate.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HsmClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificatePublicKey', 'hsmClientCertificate_hsmClientCertificatePublicKey' - The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
--
-- 'hsmClientCertificateIdentifier', 'hsmClientCertificate_hsmClientCertificateIdentifier' - The identifier of the HSM client certificate.
--
-- 'tags', 'hsmClientCertificate_tags' - The list of tags for the HSM client certificate.
newHsmClientCertificate ::
  HsmClientCertificate
newHsmClientCertificate =
  HsmClientCertificate'
    { hsmClientCertificatePublicKey =
        Core.Nothing,
      hsmClientCertificateIdentifier = Core.Nothing,
      tags = Core.Nothing
    }

-- | The public key that the Amazon Redshift cluster will use to connect to
-- the HSM. You must register the public key in the HSM.
hsmClientCertificate_hsmClientCertificatePublicKey :: Lens.Lens' HsmClientCertificate (Core.Maybe Core.Text)
hsmClientCertificate_hsmClientCertificatePublicKey = Lens.lens (\HsmClientCertificate' {hsmClientCertificatePublicKey} -> hsmClientCertificatePublicKey) (\s@HsmClientCertificate' {} a -> s {hsmClientCertificatePublicKey = a} :: HsmClientCertificate)

-- | The identifier of the HSM client certificate.
hsmClientCertificate_hsmClientCertificateIdentifier :: Lens.Lens' HsmClientCertificate (Core.Maybe Core.Text)
hsmClientCertificate_hsmClientCertificateIdentifier = Lens.lens (\HsmClientCertificate' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@HsmClientCertificate' {} a -> s {hsmClientCertificateIdentifier = a} :: HsmClientCertificate)

-- | The list of tags for the HSM client certificate.
hsmClientCertificate_tags :: Lens.Lens' HsmClientCertificate (Core.Maybe [Tag])
hsmClientCertificate_tags = Lens.lens (\HsmClientCertificate' {tags} -> tags) (\s@HsmClientCertificate' {} a -> s {tags = a} :: HsmClientCertificate) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML HsmClientCertificate where
  parseXML x =
    HsmClientCertificate'
      Core.<$> (x Core..@? "HsmClientCertificatePublicKey")
      Core.<*> (x Core..@? "HsmClientCertificateIdentifier")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )

instance Core.Hashable HsmClientCertificate

instance Core.NFData HsmClientCertificate
