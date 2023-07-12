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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.GeneralName where

import Amazonka.CertificateManagerPCA.Types.ASN1Subject
import Amazonka.CertificateManagerPCA.Types.EdiPartyName
import Amazonka.CertificateManagerPCA.Types.OtherName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an ASN.1 X.400 @GeneralName@ as defined in
-- <https://datatracker.ietf.org/doc/html/rfc5280 RFC 5280>. Only one of
-- the following naming options should be provided. Providing more than one
-- option results in an @InvalidArgsException@ error.
--
-- /See:/ 'newGeneralName' smart constructor.
data GeneralName = GeneralName'
  { directoryName :: Prelude.Maybe ASN1Subject,
    -- | Represents @GeneralName@ as a DNS name.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as an @EdiPartyName@ object.
    ediPartyName :: Prelude.Maybe EdiPartyName,
    -- | Represents @GeneralName@ as an IPv4 or IPv6 address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ using an @OtherName@ object.
    otherName :: Prelude.Maybe OtherName,
    -- | Represents @GeneralName@ as an object identifier (OID).
    registeredId :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as an
    -- <https://datatracker.ietf.org/doc/html/rfc822 RFC 822> email address.
    rfc822Name :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as a URI.
    uniformResourceIdentifier :: Prelude.Maybe Prelude.Text
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
-- 'directoryName', 'generalName_directoryName' - Undocumented member.
--
-- 'dnsName', 'generalName_dnsName' - Represents @GeneralName@ as a DNS name.
--
-- 'ediPartyName', 'generalName_ediPartyName' - Represents @GeneralName@ as an @EdiPartyName@ object.
--
-- 'ipAddress', 'generalName_ipAddress' - Represents @GeneralName@ as an IPv4 or IPv6 address.
--
-- 'otherName', 'generalName_otherName' - Represents @GeneralName@ using an @OtherName@ object.
--
-- 'registeredId', 'generalName_registeredId' - Represents @GeneralName@ as an object identifier (OID).
--
-- 'rfc822Name', 'generalName_rfc822Name' - Represents @GeneralName@ as an
-- <https://datatracker.ietf.org/doc/html/rfc822 RFC 822> email address.
--
-- 'uniformResourceIdentifier', 'generalName_uniformResourceIdentifier' - Represents @GeneralName@ as a URI.
newGeneralName ::
  GeneralName
newGeneralName =
  GeneralName'
    { directoryName = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      ediPartyName = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      otherName = Prelude.Nothing,
      registeredId = Prelude.Nothing,
      rfc822Name = Prelude.Nothing,
      uniformResourceIdentifier = Prelude.Nothing
    }

-- | Undocumented member.
generalName_directoryName :: Lens.Lens' GeneralName (Prelude.Maybe ASN1Subject)
generalName_directoryName = Lens.lens (\GeneralName' {directoryName} -> directoryName) (\s@GeneralName' {} a -> s {directoryName = a} :: GeneralName)

-- | Represents @GeneralName@ as a DNS name.
generalName_dnsName :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_dnsName = Lens.lens (\GeneralName' {dnsName} -> dnsName) (\s@GeneralName' {} a -> s {dnsName = a} :: GeneralName)

-- | Represents @GeneralName@ as an @EdiPartyName@ object.
generalName_ediPartyName :: Lens.Lens' GeneralName (Prelude.Maybe EdiPartyName)
generalName_ediPartyName = Lens.lens (\GeneralName' {ediPartyName} -> ediPartyName) (\s@GeneralName' {} a -> s {ediPartyName = a} :: GeneralName)

-- | Represents @GeneralName@ as an IPv4 or IPv6 address.
generalName_ipAddress :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_ipAddress = Lens.lens (\GeneralName' {ipAddress} -> ipAddress) (\s@GeneralName' {} a -> s {ipAddress = a} :: GeneralName)

-- | Represents @GeneralName@ using an @OtherName@ object.
generalName_otherName :: Lens.Lens' GeneralName (Prelude.Maybe OtherName)
generalName_otherName = Lens.lens (\GeneralName' {otherName} -> otherName) (\s@GeneralName' {} a -> s {otherName = a} :: GeneralName)

-- | Represents @GeneralName@ as an object identifier (OID).
generalName_registeredId :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_registeredId = Lens.lens (\GeneralName' {registeredId} -> registeredId) (\s@GeneralName' {} a -> s {registeredId = a} :: GeneralName)

-- | Represents @GeneralName@ as an
-- <https://datatracker.ietf.org/doc/html/rfc822 RFC 822> email address.
generalName_rfc822Name :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_rfc822Name = Lens.lens (\GeneralName' {rfc822Name} -> rfc822Name) (\s@GeneralName' {} a -> s {rfc822Name = a} :: GeneralName)

-- | Represents @GeneralName@ as a URI.
generalName_uniformResourceIdentifier :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_uniformResourceIdentifier = Lens.lens (\GeneralName' {uniformResourceIdentifier} -> uniformResourceIdentifier) (\s@GeneralName' {} a -> s {uniformResourceIdentifier = a} :: GeneralName)

instance Data.FromJSON GeneralName where
  parseJSON =
    Data.withObject
      "GeneralName"
      ( \x ->
          GeneralName'
            Prelude.<$> (x Data..:? "DirectoryName")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "EdiPartyName")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "OtherName")
            Prelude.<*> (x Data..:? "RegisteredId")
            Prelude.<*> (x Data..:? "Rfc822Name")
            Prelude.<*> (x Data..:? "UniformResourceIdentifier")
      )

instance Prelude.Hashable GeneralName where
  hashWithSalt _salt GeneralName' {..} =
    _salt
      `Prelude.hashWithSalt` directoryName
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` ediPartyName
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` otherName
      `Prelude.hashWithSalt` registeredId
      `Prelude.hashWithSalt` rfc822Name
      `Prelude.hashWithSalt` uniformResourceIdentifier

instance Prelude.NFData GeneralName where
  rnf GeneralName' {..} =
    Prelude.rnf directoryName
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf ediPartyName
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf otherName
      `Prelude.seq` Prelude.rnf registeredId
      `Prelude.seq` Prelude.rnf rfc822Name
      `Prelude.seq` Prelude.rnf uniformResourceIdentifier

instance Data.ToJSON GeneralName where
  toJSON GeneralName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryName" Data..=) Prelude.<$> directoryName,
            ("DnsName" Data..=) Prelude.<$> dnsName,
            ("EdiPartyName" Data..=) Prelude.<$> ediPartyName,
            ("IpAddress" Data..=) Prelude.<$> ipAddress,
            ("OtherName" Data..=) Prelude.<$> otherName,
            ("RegisteredId" Data..=) Prelude.<$> registeredId,
            ("Rfc822Name" Data..=) Prelude.<$> rfc822Name,
            ("UniformResourceIdentifier" Data..=)
              Prelude.<$> uniformResourceIdentifier
          ]
      )
