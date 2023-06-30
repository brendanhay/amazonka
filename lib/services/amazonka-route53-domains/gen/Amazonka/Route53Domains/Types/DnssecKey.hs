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
-- Module      : Amazonka.Route53Domains.Types.DnssecKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.DnssecKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the DNSSEC key.
--
-- You get this from your DNS provider and then give it to Route 53 (by
-- using
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AssociateDelegationSignerToDomain.html AssociateDelegationSignerToDomain>)
-- to pass it to the registry to establish the chain of trust.
--
-- /See:/ 'newDnssecKey' smart constructor.
data DnssecKey = DnssecKey'
  { -- | The number of the public key’s cryptographic algorithm according to an
    -- <https://www.iana.org/assignments/dns-sec-alg-numbers/dns-sec-alg-numbers.xml IANA>
    -- assignment.
    --
    -- If Route 53 is your DNS service, set this to 13.
    --
    -- For more information about enabling DNSSEC signing, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-configuring-dnssec-enable-signing.html Enabling DNSSEC signing and establishing a chain of trust>.
    algorithm :: Prelude.Maybe Prelude.Int,
    -- | The delegation signer digest.
    --
    -- Digest is calculated from the public key provided using specified digest
    -- algorithm and this digest is the actual value returned from the registry
    -- nameservers as the value of DS records.
    digest :: Prelude.Maybe Prelude.Text,
    -- | The number of the DS digest algorithm according to an IANA assignment.
    --
    -- For more information, see
    -- <https://www.iana.org/assignments/ds-rr-types/ds-rr-types.xhtml IANA>
    -- for DNSSEC Delegation Signer (DS) Resource Record (RR) Type Digest
    -- Algorithms.
    digestType :: Prelude.Maybe Prelude.Int,
    -- | Defines the type of key. It can be either a KSK (key-signing-key, value
    -- 257) or ZSK (zone-signing-key, value 256). Using KSK is always
    -- encouraged. Only use ZSK if your DNS provider isn\'t Route 53 and you
    -- don’t have KSK available.
    --
    -- If you have KSK and ZSK keys, always use KSK to create a delegations
    -- signer (DS) record. If you have ZSK keys only – use ZSK to create a DS
    -- record.
    flags :: Prelude.Maybe Prelude.Int,
    -- | An ID assigned to each DS record created by
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AssociateDelegationSignerToDomain.html AssociateDelegationSignerToDomain>.
    id :: Prelude.Maybe Prelude.Text,
    -- | A numeric identification of the DNSKEY record referred to by this DS
    -- record.
    keyTag :: Prelude.Maybe Prelude.Int,
    -- | The base64-encoded public key part of the key pair that is passed to the
    -- registry .
    publicKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnssecKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'dnssecKey_algorithm' - The number of the public key’s cryptographic algorithm according to an
-- <https://www.iana.org/assignments/dns-sec-alg-numbers/dns-sec-alg-numbers.xml IANA>
-- assignment.
--
-- If Route 53 is your DNS service, set this to 13.
--
-- For more information about enabling DNSSEC signing, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-configuring-dnssec-enable-signing.html Enabling DNSSEC signing and establishing a chain of trust>.
--
-- 'digest', 'dnssecKey_digest' - The delegation signer digest.
--
-- Digest is calculated from the public key provided using specified digest
-- algorithm and this digest is the actual value returned from the registry
-- nameservers as the value of DS records.
--
-- 'digestType', 'dnssecKey_digestType' - The number of the DS digest algorithm according to an IANA assignment.
--
-- For more information, see
-- <https://www.iana.org/assignments/ds-rr-types/ds-rr-types.xhtml IANA>
-- for DNSSEC Delegation Signer (DS) Resource Record (RR) Type Digest
-- Algorithms.
--
-- 'flags', 'dnssecKey_flags' - Defines the type of key. It can be either a KSK (key-signing-key, value
-- 257) or ZSK (zone-signing-key, value 256). Using KSK is always
-- encouraged. Only use ZSK if your DNS provider isn\'t Route 53 and you
-- don’t have KSK available.
--
-- If you have KSK and ZSK keys, always use KSK to create a delegations
-- signer (DS) record. If you have ZSK keys only – use ZSK to create a DS
-- record.
--
-- 'id', 'dnssecKey_id' - An ID assigned to each DS record created by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AssociateDelegationSignerToDomain.html AssociateDelegationSignerToDomain>.
--
-- 'keyTag', 'dnssecKey_keyTag' - A numeric identification of the DNSKEY record referred to by this DS
-- record.
--
-- 'publicKey', 'dnssecKey_publicKey' - The base64-encoded public key part of the key pair that is passed to the
-- registry .
newDnssecKey ::
  DnssecKey
newDnssecKey =
  DnssecKey'
    { algorithm = Prelude.Nothing,
      digest = Prelude.Nothing,
      digestType = Prelude.Nothing,
      flags = Prelude.Nothing,
      id = Prelude.Nothing,
      keyTag = Prelude.Nothing,
      publicKey = Prelude.Nothing
    }

-- | The number of the public key’s cryptographic algorithm according to an
-- <https://www.iana.org/assignments/dns-sec-alg-numbers/dns-sec-alg-numbers.xml IANA>
-- assignment.
--
-- If Route 53 is your DNS service, set this to 13.
--
-- For more information about enabling DNSSEC signing, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-configuring-dnssec-enable-signing.html Enabling DNSSEC signing and establishing a chain of trust>.
dnssecKey_algorithm :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Int)
dnssecKey_algorithm = Lens.lens (\DnssecKey' {algorithm} -> algorithm) (\s@DnssecKey' {} a -> s {algorithm = a} :: DnssecKey)

-- | The delegation signer digest.
--
-- Digest is calculated from the public key provided using specified digest
-- algorithm and this digest is the actual value returned from the registry
-- nameservers as the value of DS records.
dnssecKey_digest :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Text)
dnssecKey_digest = Lens.lens (\DnssecKey' {digest} -> digest) (\s@DnssecKey' {} a -> s {digest = a} :: DnssecKey)

-- | The number of the DS digest algorithm according to an IANA assignment.
--
-- For more information, see
-- <https://www.iana.org/assignments/ds-rr-types/ds-rr-types.xhtml IANA>
-- for DNSSEC Delegation Signer (DS) Resource Record (RR) Type Digest
-- Algorithms.
dnssecKey_digestType :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Int)
dnssecKey_digestType = Lens.lens (\DnssecKey' {digestType} -> digestType) (\s@DnssecKey' {} a -> s {digestType = a} :: DnssecKey)

-- | Defines the type of key. It can be either a KSK (key-signing-key, value
-- 257) or ZSK (zone-signing-key, value 256). Using KSK is always
-- encouraged. Only use ZSK if your DNS provider isn\'t Route 53 and you
-- don’t have KSK available.
--
-- If you have KSK and ZSK keys, always use KSK to create a delegations
-- signer (DS) record. If you have ZSK keys only – use ZSK to create a DS
-- record.
dnssecKey_flags :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Int)
dnssecKey_flags = Lens.lens (\DnssecKey' {flags} -> flags) (\s@DnssecKey' {} a -> s {flags = a} :: DnssecKey)

-- | An ID assigned to each DS record created by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AssociateDelegationSignerToDomain.html AssociateDelegationSignerToDomain>.
dnssecKey_id :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Text)
dnssecKey_id = Lens.lens (\DnssecKey' {id} -> id) (\s@DnssecKey' {} a -> s {id = a} :: DnssecKey)

-- | A numeric identification of the DNSKEY record referred to by this DS
-- record.
dnssecKey_keyTag :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Int)
dnssecKey_keyTag = Lens.lens (\DnssecKey' {keyTag} -> keyTag) (\s@DnssecKey' {} a -> s {keyTag = a} :: DnssecKey)

-- | The base64-encoded public key part of the key pair that is passed to the
-- registry .
dnssecKey_publicKey :: Lens.Lens' DnssecKey (Prelude.Maybe Prelude.Text)
dnssecKey_publicKey = Lens.lens (\DnssecKey' {publicKey} -> publicKey) (\s@DnssecKey' {} a -> s {publicKey = a} :: DnssecKey)

instance Data.FromJSON DnssecKey where
  parseJSON =
    Data.withObject
      "DnssecKey"
      ( \x ->
          DnssecKey'
            Prelude.<$> (x Data..:? "Algorithm")
            Prelude.<*> (x Data..:? "Digest")
            Prelude.<*> (x Data..:? "DigestType")
            Prelude.<*> (x Data..:? "Flags")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "KeyTag")
            Prelude.<*> (x Data..:? "PublicKey")
      )

instance Prelude.Hashable DnssecKey where
  hashWithSalt _salt DnssecKey' {..} =
    _salt
      `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` digest
      `Prelude.hashWithSalt` digestType
      `Prelude.hashWithSalt` flags
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` keyTag
      `Prelude.hashWithSalt` publicKey

instance Prelude.NFData DnssecKey where
  rnf DnssecKey' {..} =
    Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf digest
      `Prelude.seq` Prelude.rnf digestType
      `Prelude.seq` Prelude.rnf flags
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf keyTag
      `Prelude.seq` Prelude.rnf publicKey
