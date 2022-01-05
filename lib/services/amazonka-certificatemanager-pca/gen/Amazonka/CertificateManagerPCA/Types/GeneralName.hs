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
-- Module      : Amazonka.CertificateManagerPCA.Types.GeneralName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.GeneralName where

import Amazonka.CertificateManagerPCA.Types.ASN1Subject
import Amazonka.CertificateManagerPCA.Types.EdiPartyName
import Amazonka.CertificateManagerPCA.Types.OtherName
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an ASN.1 X.400 @GeneralName@ as defined in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>. Only one of the
-- following naming options should be provided. Providing more than one
-- option results in an @InvalidArgsException@ error.
--
-- /See:/ 'newGeneralName' smart constructor.
data GeneralName = GeneralName'
  { -- | Represents @GeneralName@ as an IPv4 or IPv6 address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as a URI.
    uniformResourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as an object identifier (OID).
    registeredId :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as an @EdiPartyName@ object.
    ediPartyName :: Prelude.Maybe EdiPartyName,
    -- | Represents @GeneralName@ as an
    -- <https://tools.ietf.org/html/rfc822 RFC 822> email address.
    rfc822Name :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ using an @OtherName@ object.
    otherName :: Prelude.Maybe OtherName,
    -- | Represents @GeneralName@ as a DNS name.
    dnsName :: Prelude.Maybe Prelude.Text,
    directoryName :: Prelude.Maybe ASN1Subject
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneralName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'generalName_ipAddress' - Represents @GeneralName@ as an IPv4 or IPv6 address.
--
-- 'uniformResourceIdentifier', 'generalName_uniformResourceIdentifier' - Represents @GeneralName@ as a URI.
--
-- 'registeredId', 'generalName_registeredId' - Represents @GeneralName@ as an object identifier (OID).
--
-- 'ediPartyName', 'generalName_ediPartyName' - Represents @GeneralName@ as an @EdiPartyName@ object.
--
-- 'rfc822Name', 'generalName_rfc822Name' - Represents @GeneralName@ as an
-- <https://tools.ietf.org/html/rfc822 RFC 822> email address.
--
-- 'otherName', 'generalName_otherName' - Represents @GeneralName@ using an @OtherName@ object.
--
-- 'dnsName', 'generalName_dnsName' - Represents @GeneralName@ as a DNS name.
--
-- 'directoryName', 'generalName_directoryName' - Undocumented member.
newGeneralName ::
  GeneralName
newGeneralName =
  GeneralName'
    { ipAddress = Prelude.Nothing,
      uniformResourceIdentifier = Prelude.Nothing,
      registeredId = Prelude.Nothing,
      ediPartyName = Prelude.Nothing,
      rfc822Name = Prelude.Nothing,
      otherName = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      directoryName = Prelude.Nothing
    }

-- | Represents @GeneralName@ as an IPv4 or IPv6 address.
generalName_ipAddress :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_ipAddress = Lens.lens (\GeneralName' {ipAddress} -> ipAddress) (\s@GeneralName' {} a -> s {ipAddress = a} :: GeneralName)

-- | Represents @GeneralName@ as a URI.
generalName_uniformResourceIdentifier :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_uniformResourceIdentifier = Lens.lens (\GeneralName' {uniformResourceIdentifier} -> uniformResourceIdentifier) (\s@GeneralName' {} a -> s {uniformResourceIdentifier = a} :: GeneralName)

-- | Represents @GeneralName@ as an object identifier (OID).
generalName_registeredId :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_registeredId = Lens.lens (\GeneralName' {registeredId} -> registeredId) (\s@GeneralName' {} a -> s {registeredId = a} :: GeneralName)

-- | Represents @GeneralName@ as an @EdiPartyName@ object.
generalName_ediPartyName :: Lens.Lens' GeneralName (Prelude.Maybe EdiPartyName)
generalName_ediPartyName = Lens.lens (\GeneralName' {ediPartyName} -> ediPartyName) (\s@GeneralName' {} a -> s {ediPartyName = a} :: GeneralName)

-- | Represents @GeneralName@ as an
-- <https://tools.ietf.org/html/rfc822 RFC 822> email address.
generalName_rfc822Name :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_rfc822Name = Lens.lens (\GeneralName' {rfc822Name} -> rfc822Name) (\s@GeneralName' {} a -> s {rfc822Name = a} :: GeneralName)

-- | Represents @GeneralName@ using an @OtherName@ object.
generalName_otherName :: Lens.Lens' GeneralName (Prelude.Maybe OtherName)
generalName_otherName = Lens.lens (\GeneralName' {otherName} -> otherName) (\s@GeneralName' {} a -> s {otherName = a} :: GeneralName)

-- | Represents @GeneralName@ as a DNS name.
generalName_dnsName :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_dnsName = Lens.lens (\GeneralName' {dnsName} -> dnsName) (\s@GeneralName' {} a -> s {dnsName = a} :: GeneralName)

-- | Undocumented member.
generalName_directoryName :: Lens.Lens' GeneralName (Prelude.Maybe ASN1Subject)
generalName_directoryName = Lens.lens (\GeneralName' {directoryName} -> directoryName) (\s@GeneralName' {} a -> s {directoryName = a} :: GeneralName)

instance Core.FromJSON GeneralName where
  parseJSON =
    Core.withObject
      "GeneralName"
      ( \x ->
          GeneralName'
            Prelude.<$> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "UniformResourceIdentifier")
            Prelude.<*> (x Core..:? "RegisteredId")
            Prelude.<*> (x Core..:? "EdiPartyName")
            Prelude.<*> (x Core..:? "Rfc822Name")
            Prelude.<*> (x Core..:? "OtherName")
            Prelude.<*> (x Core..:? "DnsName")
            Prelude.<*> (x Core..:? "DirectoryName")
      )

instance Prelude.Hashable GeneralName where
  hashWithSalt _salt GeneralName' {..} =
    _salt `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` uniformResourceIdentifier
      `Prelude.hashWithSalt` registeredId
      `Prelude.hashWithSalt` ediPartyName
      `Prelude.hashWithSalt` rfc822Name
      `Prelude.hashWithSalt` otherName
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` directoryName

instance Prelude.NFData GeneralName where
  rnf GeneralName' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf uniformResourceIdentifier
      `Prelude.seq` Prelude.rnf registeredId
      `Prelude.seq` Prelude.rnf ediPartyName
      `Prelude.seq` Prelude.rnf rfc822Name
      `Prelude.seq` Prelude.rnf otherName
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf directoryName

instance Core.ToJSON GeneralName where
  toJSON GeneralName' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IpAddress" Core..=) Prelude.<$> ipAddress,
            ("UniformResourceIdentifier" Core..=)
              Prelude.<$> uniformResourceIdentifier,
            ("RegisteredId" Core..=) Prelude.<$> registeredId,
            ("EdiPartyName" Core..=) Prelude.<$> ediPartyName,
            ("Rfc822Name" Core..=) Prelude.<$> rfc822Name,
            ("OtherName" Core..=) Prelude.<$> otherName,
            ("DnsName" Core..=) Prelude.<$> dnsName,
            ("DirectoryName" Core..=) Prelude.<$> directoryName
          ]
      )
