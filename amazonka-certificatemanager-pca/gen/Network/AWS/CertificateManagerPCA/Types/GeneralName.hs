{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManagerPCA.Types.GeneralName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.GeneralName where

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.EdiPartyName
import Network.AWS.CertificateManagerPCA.Types.OtherName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an ASN.1 X.400 @GeneralName@ as defined in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>. Only one of the
-- following naming options should be provided. Providing more than one
-- option results in an @InvalidArgsException@ error.
--
-- /See:/ 'newGeneralName' smart constructor.
data GeneralName = GeneralName'
  { -- | Represents @GeneralName@ as an @EdiPartyName@ object.
    ediPartyName :: Prelude.Maybe EdiPartyName,
    -- | Represents @GeneralName@ using an @OtherName@ object.
    otherName :: Prelude.Maybe OtherName,
    -- | Represents @GeneralName@ as a URI.
    uniformResourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as an IPv4 or IPv6 address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as a DNS name.
    dnsName :: Prelude.Maybe Prelude.Text,
    directoryName :: Prelude.Maybe ASN1Subject,
    -- | Represents @GeneralName@ as an
    -- <https://tools.ietf.org/html/rfc822 RFC 822> email address.
    rfc822Name :: Prelude.Maybe Prelude.Text,
    -- | Represents @GeneralName@ as an object identifier (OID).
    registeredId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GeneralName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ediPartyName', 'generalName_ediPartyName' - Represents @GeneralName@ as an @EdiPartyName@ object.
--
-- 'otherName', 'generalName_otherName' - Represents @GeneralName@ using an @OtherName@ object.
--
-- 'uniformResourceIdentifier', 'generalName_uniformResourceIdentifier' - Represents @GeneralName@ as a URI.
--
-- 'ipAddress', 'generalName_ipAddress' - Represents @GeneralName@ as an IPv4 or IPv6 address.
--
-- 'dnsName', 'generalName_dnsName' - Represents @GeneralName@ as a DNS name.
--
-- 'directoryName', 'generalName_directoryName' - Undocumented member.
--
-- 'rfc822Name', 'generalName_rfc822Name' - Represents @GeneralName@ as an
-- <https://tools.ietf.org/html/rfc822 RFC 822> email address.
--
-- 'registeredId', 'generalName_registeredId' - Represents @GeneralName@ as an object identifier (OID).
newGeneralName ::
  GeneralName
newGeneralName =
  GeneralName'
    { ediPartyName = Prelude.Nothing,
      otherName = Prelude.Nothing,
      uniformResourceIdentifier = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      directoryName = Prelude.Nothing,
      rfc822Name = Prelude.Nothing,
      registeredId = Prelude.Nothing
    }

-- | Represents @GeneralName@ as an @EdiPartyName@ object.
generalName_ediPartyName :: Lens.Lens' GeneralName (Prelude.Maybe EdiPartyName)
generalName_ediPartyName = Lens.lens (\GeneralName' {ediPartyName} -> ediPartyName) (\s@GeneralName' {} a -> s {ediPartyName = a} :: GeneralName)

-- | Represents @GeneralName@ using an @OtherName@ object.
generalName_otherName :: Lens.Lens' GeneralName (Prelude.Maybe OtherName)
generalName_otherName = Lens.lens (\GeneralName' {otherName} -> otherName) (\s@GeneralName' {} a -> s {otherName = a} :: GeneralName)

-- | Represents @GeneralName@ as a URI.
generalName_uniformResourceIdentifier :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_uniformResourceIdentifier = Lens.lens (\GeneralName' {uniformResourceIdentifier} -> uniformResourceIdentifier) (\s@GeneralName' {} a -> s {uniformResourceIdentifier = a} :: GeneralName)

-- | Represents @GeneralName@ as an IPv4 or IPv6 address.
generalName_ipAddress :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_ipAddress = Lens.lens (\GeneralName' {ipAddress} -> ipAddress) (\s@GeneralName' {} a -> s {ipAddress = a} :: GeneralName)

-- | Represents @GeneralName@ as a DNS name.
generalName_dnsName :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_dnsName = Lens.lens (\GeneralName' {dnsName} -> dnsName) (\s@GeneralName' {} a -> s {dnsName = a} :: GeneralName)

-- | Undocumented member.
generalName_directoryName :: Lens.Lens' GeneralName (Prelude.Maybe ASN1Subject)
generalName_directoryName = Lens.lens (\GeneralName' {directoryName} -> directoryName) (\s@GeneralName' {} a -> s {directoryName = a} :: GeneralName)

-- | Represents @GeneralName@ as an
-- <https://tools.ietf.org/html/rfc822 RFC 822> email address.
generalName_rfc822Name :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_rfc822Name = Lens.lens (\GeneralName' {rfc822Name} -> rfc822Name) (\s@GeneralName' {} a -> s {rfc822Name = a} :: GeneralName)

-- | Represents @GeneralName@ as an object identifier (OID).
generalName_registeredId :: Lens.Lens' GeneralName (Prelude.Maybe Prelude.Text)
generalName_registeredId = Lens.lens (\GeneralName' {registeredId} -> registeredId) (\s@GeneralName' {} a -> s {registeredId = a} :: GeneralName)

instance Prelude.FromJSON GeneralName where
  parseJSON =
    Prelude.withObject
      "GeneralName"
      ( \x ->
          GeneralName'
            Prelude.<$> (x Prelude..:? "EdiPartyName")
            Prelude.<*> (x Prelude..:? "OtherName")
            Prelude.<*> (x Prelude..:? "UniformResourceIdentifier")
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> (x Prelude..:? "DnsName")
            Prelude.<*> (x Prelude..:? "DirectoryName")
            Prelude.<*> (x Prelude..:? "Rfc822Name")
            Prelude.<*> (x Prelude..:? "RegisteredId")
      )

instance Prelude.Hashable GeneralName

instance Prelude.NFData GeneralName

instance Prelude.ToJSON GeneralName where
  toJSON GeneralName' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EdiPartyName" Prelude..=)
              Prelude.<$> ediPartyName,
            ("OtherName" Prelude..=) Prelude.<$> otherName,
            ("UniformResourceIdentifier" Prelude..=)
              Prelude.<$> uniformResourceIdentifier,
            ("IpAddress" Prelude..=) Prelude.<$> ipAddress,
            ("DnsName" Prelude..=) Prelude.<$> dnsName,
            ("DirectoryName" Prelude..=)
              Prelude.<$> directoryName,
            ("Rfc822Name" Prelude..=) Prelude.<$> rfc822Name,
            ("RegisteredId" Prelude..=)
              Prelude.<$> registeredId
          ]
      )
