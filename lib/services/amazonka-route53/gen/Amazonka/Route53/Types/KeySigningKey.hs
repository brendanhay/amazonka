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
-- Module      : Amazonka.Route53.Types.KeySigningKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.KeySigningKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A key-signing key (KSK) is a complex type that represents a
-- public\/private key pair. The private key is used to generate a digital
-- signature for the zone signing key (ZSK). The public key is stored in
-- the DNS and is used to authenticate the ZSK. A KSK is always associated
-- with a hosted zone; it cannot exist by itself.
--
-- /See:/ 'newKeySigningKey' smart constructor.
data KeySigningKey = KeySigningKey'
  { -- | The date when the key-signing key (KSK) was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | A string that represents a DNSKEY record.
    dNSKEYRecord :: Prelude.Maybe Prelude.Text,
    -- | A string that represents a delegation signer (DS) record.
    dSRecord :: Prelude.Maybe Prelude.Text,
    -- | A string used to represent the delegation signer digest algorithm. This
    -- value must follow the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
    digestAlgorithmMnemonic :: Prelude.Maybe Prelude.Text,
    -- | An integer used to represent the delegation signer digest algorithm.
    -- This value must follow the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
    digestAlgorithmType :: Prelude.Maybe Prelude.Int,
    -- | A cryptographic digest of a DNSKEY resource record (RR). DNSKEY records
    -- are used to publish the public key that resolvers can use to verify
    -- DNSSEC signatures that are used to secure certain kinds of information
    -- provided by the DNS system.
    digestValue :: Prelude.Maybe Prelude.Text,
    -- | An integer that specifies how the key is used. For key-signing key
    -- (KSK), this value is always 257.
    flag :: Prelude.Maybe Prelude.Int,
    -- | An integer used to identify the DNSSEC record for the domain name. The
    -- process used to calculate the value is described in
    -- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Appendix B>.
    keyTag :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon resource name (ARN) used to identify the customer managed key
    -- in Key Management Service (KMS). The @KmsArn@ must be unique for each
    -- key-signing key (KSK) in a single hosted zone.
    --
    -- You must configure the customer managed key as follows:
    --
    -- [Status]
    --     Enabled
    --
    -- [Key spec]
    --     ECC_NIST_P256
    --
    -- [Key usage]
    --     Sign and verify
    --
    -- [Key policy]
    --     The key policy must give permission for the following actions:
    --
    --     -   DescribeKey
    --
    --     -   GetPublicKey
    --
    --     -   Sign
    --
    --     The key policy must also include the Amazon Route 53 service in the
    --     principal for your account. Specify the following:
    --
    --     -   @\"Service\": \"dnssec-route53.amazonaws.com\"@
    --
    -- For more information about working with the customer managed key in KMS,
    -- see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html Key Management Service concepts>.
    kmsArn :: Prelude.Maybe Prelude.Text,
    -- | The last time that the key-signing key (KSK) was changed.
    lastModifiedDate :: Prelude.Maybe Data.ISO8601,
    -- | A string used to identify a key-signing key (KSK). @Name@ can include
    -- numbers, letters, and underscores (_). @Name@ must be unique for each
    -- key-signing key in the same hosted zone.
    name :: Prelude.Maybe Prelude.Text,
    -- | The public key, represented as a Base64 encoding, as required by
    -- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Page 5>.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | A string used to represent the signing algorithm. This value must follow
    -- the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
    signingAlgorithmMnemonic :: Prelude.Maybe Prelude.Text,
    -- | An integer used to represent the signing algorithm. This value must
    -- follow the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
    signingAlgorithmType :: Prelude.Maybe Prelude.Int,
    -- | A string that represents the current key-signing key (KSK) status.
    --
    -- Status can have one of the following values:
    --
    -- [ACTIVE]
    --     The KSK is being used for signing.
    --
    -- [INACTIVE]
    --     The KSK is not being used for signing.
    --
    -- [DELETING]
    --     The KSK is in the process of being deleted.
    --
    -- [ACTION_NEEDED]
    --     There is a problem with the KSK that requires you to take action to
    --     resolve. For example, the customer managed key might have been
    --     deleted, or the permissions for the customer managed key might have
    --     been changed.
    --
    -- [INTERNAL_FAILURE]
    --     There was an error during a request. Before you can continue to work
    --     with DNSSEC signing, including actions that involve this KSK, you
    --     must correct the problem. For example, you may need to activate or
    --     deactivate the KSK.
    status :: Prelude.Maybe Prelude.Text,
    -- | The status message provided for the following key-signing key (KSK)
    -- statuses: @ACTION_NEEDED@ or @INTERNAL_FAILURE@. The status message
    -- includes information about what the problem might be and steps that you
    -- can take to correct the issue.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeySigningKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'keySigningKey_createdDate' - The date when the key-signing key (KSK) was created.
--
-- 'dNSKEYRecord', 'keySigningKey_dNSKEYRecord' - A string that represents a DNSKEY record.
--
-- 'dSRecord', 'keySigningKey_dSRecord' - A string that represents a delegation signer (DS) record.
--
-- 'digestAlgorithmMnemonic', 'keySigningKey_digestAlgorithmMnemonic' - A string used to represent the delegation signer digest algorithm. This
-- value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
--
-- 'digestAlgorithmType', 'keySigningKey_digestAlgorithmType' - An integer used to represent the delegation signer digest algorithm.
-- This value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
--
-- 'digestValue', 'keySigningKey_digestValue' - A cryptographic digest of a DNSKEY resource record (RR). DNSKEY records
-- are used to publish the public key that resolvers can use to verify
-- DNSSEC signatures that are used to secure certain kinds of information
-- provided by the DNS system.
--
-- 'flag', 'keySigningKey_flag' - An integer that specifies how the key is used. For key-signing key
-- (KSK), this value is always 257.
--
-- 'keyTag', 'keySigningKey_keyTag' - An integer used to identify the DNSSEC record for the domain name. The
-- process used to calculate the value is described in
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Appendix B>.
--
-- 'kmsArn', 'keySigningKey_kmsArn' - The Amazon resource name (ARN) used to identify the customer managed key
-- in Key Management Service (KMS). The @KmsArn@ must be unique for each
-- key-signing key (KSK) in a single hosted zone.
--
-- You must configure the customer managed key as follows:
--
-- [Status]
--     Enabled
--
-- [Key spec]
--     ECC_NIST_P256
--
-- [Key usage]
--     Sign and verify
--
-- [Key policy]
--     The key policy must give permission for the following actions:
--
--     -   DescribeKey
--
--     -   GetPublicKey
--
--     -   Sign
--
--     The key policy must also include the Amazon Route 53 service in the
--     principal for your account. Specify the following:
--
--     -   @\"Service\": \"dnssec-route53.amazonaws.com\"@
--
-- For more information about working with the customer managed key in KMS,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html Key Management Service concepts>.
--
-- 'lastModifiedDate', 'keySigningKey_lastModifiedDate' - The last time that the key-signing key (KSK) was changed.
--
-- 'name', 'keySigningKey_name' - A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
--
-- 'publicKey', 'keySigningKey_publicKey' - The public key, represented as a Base64 encoding, as required by
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Page 5>.
--
-- 'signingAlgorithmMnemonic', 'keySigningKey_signingAlgorithmMnemonic' - A string used to represent the signing algorithm. This value must follow
-- the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
--
-- 'signingAlgorithmType', 'keySigningKey_signingAlgorithmType' - An integer used to represent the signing algorithm. This value must
-- follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
--
-- 'status', 'keySigningKey_status' - A string that represents the current key-signing key (KSK) status.
--
-- Status can have one of the following values:
--
-- [ACTIVE]
--     The KSK is being used for signing.
--
-- [INACTIVE]
--     The KSK is not being used for signing.
--
-- [DELETING]
--     The KSK is in the process of being deleted.
--
-- [ACTION_NEEDED]
--     There is a problem with the KSK that requires you to take action to
--     resolve. For example, the customer managed key might have been
--     deleted, or the permissions for the customer managed key might have
--     been changed.
--
-- [INTERNAL_FAILURE]
--     There was an error during a request. Before you can continue to work
--     with DNSSEC signing, including actions that involve this KSK, you
--     must correct the problem. For example, you may need to activate or
--     deactivate the KSK.
--
-- 'statusMessage', 'keySigningKey_statusMessage' - The status message provided for the following key-signing key (KSK)
-- statuses: @ACTION_NEEDED@ or @INTERNAL_FAILURE@. The status message
-- includes information about what the problem might be and steps that you
-- can take to correct the issue.
newKeySigningKey ::
  KeySigningKey
newKeySigningKey =
  KeySigningKey'
    { createdDate = Prelude.Nothing,
      dNSKEYRecord = Prelude.Nothing,
      dSRecord = Prelude.Nothing,
      digestAlgorithmMnemonic = Prelude.Nothing,
      digestAlgorithmType = Prelude.Nothing,
      digestValue = Prelude.Nothing,
      flag = Prelude.Nothing,
      keyTag = Prelude.Nothing,
      kmsArn = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      signingAlgorithmMnemonic = Prelude.Nothing,
      signingAlgorithmType = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The date when the key-signing key (KSK) was created.
keySigningKey_createdDate :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.UTCTime)
keySigningKey_createdDate = Lens.lens (\KeySigningKey' {createdDate} -> createdDate) (\s@KeySigningKey' {} a -> s {createdDate = a} :: KeySigningKey) Prelude.. Lens.mapping Data._Time

-- | A string that represents a DNSKEY record.
keySigningKey_dNSKEYRecord :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_dNSKEYRecord = Lens.lens (\KeySigningKey' {dNSKEYRecord} -> dNSKEYRecord) (\s@KeySigningKey' {} a -> s {dNSKEYRecord = a} :: KeySigningKey)

-- | A string that represents a delegation signer (DS) record.
keySigningKey_dSRecord :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_dSRecord = Lens.lens (\KeySigningKey' {dSRecord} -> dSRecord) (\s@KeySigningKey' {} a -> s {dSRecord = a} :: KeySigningKey)

-- | A string used to represent the delegation signer digest algorithm. This
-- value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
keySigningKey_digestAlgorithmMnemonic :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_digestAlgorithmMnemonic = Lens.lens (\KeySigningKey' {digestAlgorithmMnemonic} -> digestAlgorithmMnemonic) (\s@KeySigningKey' {} a -> s {digestAlgorithmMnemonic = a} :: KeySigningKey)

-- | An integer used to represent the delegation signer digest algorithm.
-- This value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
keySigningKey_digestAlgorithmType :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Int)
keySigningKey_digestAlgorithmType = Lens.lens (\KeySigningKey' {digestAlgorithmType} -> digestAlgorithmType) (\s@KeySigningKey' {} a -> s {digestAlgorithmType = a} :: KeySigningKey)

-- | A cryptographic digest of a DNSKEY resource record (RR). DNSKEY records
-- are used to publish the public key that resolvers can use to verify
-- DNSSEC signatures that are used to secure certain kinds of information
-- provided by the DNS system.
keySigningKey_digestValue :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_digestValue = Lens.lens (\KeySigningKey' {digestValue} -> digestValue) (\s@KeySigningKey' {} a -> s {digestValue = a} :: KeySigningKey)

-- | An integer that specifies how the key is used. For key-signing key
-- (KSK), this value is always 257.
keySigningKey_flag :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Int)
keySigningKey_flag = Lens.lens (\KeySigningKey' {flag} -> flag) (\s@KeySigningKey' {} a -> s {flag = a} :: KeySigningKey)

-- | An integer used to identify the DNSSEC record for the domain name. The
-- process used to calculate the value is described in
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Appendix B>.
keySigningKey_keyTag :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Natural)
keySigningKey_keyTag = Lens.lens (\KeySigningKey' {keyTag} -> keyTag) (\s@KeySigningKey' {} a -> s {keyTag = a} :: KeySigningKey)

-- | The Amazon resource name (ARN) used to identify the customer managed key
-- in Key Management Service (KMS). The @KmsArn@ must be unique for each
-- key-signing key (KSK) in a single hosted zone.
--
-- You must configure the customer managed key as follows:
--
-- [Status]
--     Enabled
--
-- [Key spec]
--     ECC_NIST_P256
--
-- [Key usage]
--     Sign and verify
--
-- [Key policy]
--     The key policy must give permission for the following actions:
--
--     -   DescribeKey
--
--     -   GetPublicKey
--
--     -   Sign
--
--     The key policy must also include the Amazon Route 53 service in the
--     principal for your account. Specify the following:
--
--     -   @\"Service\": \"dnssec-route53.amazonaws.com\"@
--
-- For more information about working with the customer managed key in KMS,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html Key Management Service concepts>.
keySigningKey_kmsArn :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_kmsArn = Lens.lens (\KeySigningKey' {kmsArn} -> kmsArn) (\s@KeySigningKey' {} a -> s {kmsArn = a} :: KeySigningKey)

-- | The last time that the key-signing key (KSK) was changed.
keySigningKey_lastModifiedDate :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.UTCTime)
keySigningKey_lastModifiedDate = Lens.lens (\KeySigningKey' {lastModifiedDate} -> lastModifiedDate) (\s@KeySigningKey' {} a -> s {lastModifiedDate = a} :: KeySigningKey) Prelude.. Lens.mapping Data._Time

-- | A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
keySigningKey_name :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_name = Lens.lens (\KeySigningKey' {name} -> name) (\s@KeySigningKey' {} a -> s {name = a} :: KeySigningKey)

-- | The public key, represented as a Base64 encoding, as required by
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Page 5>.
keySigningKey_publicKey :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_publicKey = Lens.lens (\KeySigningKey' {publicKey} -> publicKey) (\s@KeySigningKey' {} a -> s {publicKey = a} :: KeySigningKey)

-- | A string used to represent the signing algorithm. This value must follow
-- the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
keySigningKey_signingAlgorithmMnemonic :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_signingAlgorithmMnemonic = Lens.lens (\KeySigningKey' {signingAlgorithmMnemonic} -> signingAlgorithmMnemonic) (\s@KeySigningKey' {} a -> s {signingAlgorithmMnemonic = a} :: KeySigningKey)

-- | An integer used to represent the signing algorithm. This value must
-- follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
keySigningKey_signingAlgorithmType :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Int)
keySigningKey_signingAlgorithmType = Lens.lens (\KeySigningKey' {signingAlgorithmType} -> signingAlgorithmType) (\s@KeySigningKey' {} a -> s {signingAlgorithmType = a} :: KeySigningKey)

-- | A string that represents the current key-signing key (KSK) status.
--
-- Status can have one of the following values:
--
-- [ACTIVE]
--     The KSK is being used for signing.
--
-- [INACTIVE]
--     The KSK is not being used for signing.
--
-- [DELETING]
--     The KSK is in the process of being deleted.
--
-- [ACTION_NEEDED]
--     There is a problem with the KSK that requires you to take action to
--     resolve. For example, the customer managed key might have been
--     deleted, or the permissions for the customer managed key might have
--     been changed.
--
-- [INTERNAL_FAILURE]
--     There was an error during a request. Before you can continue to work
--     with DNSSEC signing, including actions that involve this KSK, you
--     must correct the problem. For example, you may need to activate or
--     deactivate the KSK.
keySigningKey_status :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_status = Lens.lens (\KeySigningKey' {status} -> status) (\s@KeySigningKey' {} a -> s {status = a} :: KeySigningKey)

-- | The status message provided for the following key-signing key (KSK)
-- statuses: @ACTION_NEEDED@ or @INTERNAL_FAILURE@. The status message
-- includes information about what the problem might be and steps that you
-- can take to correct the issue.
keySigningKey_statusMessage :: Lens.Lens' KeySigningKey (Prelude.Maybe Prelude.Text)
keySigningKey_statusMessage = Lens.lens (\KeySigningKey' {statusMessage} -> statusMessage) (\s@KeySigningKey' {} a -> s {statusMessage = a} :: KeySigningKey)

instance Data.FromXML KeySigningKey where
  parseXML x =
    KeySigningKey'
      Prelude.<$> (x Data..@? "CreatedDate")
      Prelude.<*> (x Data..@? "DNSKEYRecord")
      Prelude.<*> (x Data..@? "DSRecord")
      Prelude.<*> (x Data..@? "DigestAlgorithmMnemonic")
      Prelude.<*> (x Data..@? "DigestAlgorithmType")
      Prelude.<*> (x Data..@? "DigestValue")
      Prelude.<*> (x Data..@? "Flag")
      Prelude.<*> (x Data..@? "KeyTag")
      Prelude.<*> (x Data..@? "KmsArn")
      Prelude.<*> (x Data..@? "LastModifiedDate")
      Prelude.<*> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "PublicKey")
      Prelude.<*> (x Data..@? "SigningAlgorithmMnemonic")
      Prelude.<*> (x Data..@? "SigningAlgorithmType")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusMessage")

instance Prelude.Hashable KeySigningKey where
  hashWithSalt _salt KeySigningKey' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` dNSKEYRecord
      `Prelude.hashWithSalt` dSRecord
      `Prelude.hashWithSalt` digestAlgorithmMnemonic
      `Prelude.hashWithSalt` digestAlgorithmType
      `Prelude.hashWithSalt` digestValue
      `Prelude.hashWithSalt` flag
      `Prelude.hashWithSalt` keyTag
      `Prelude.hashWithSalt` kmsArn
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` publicKey
      `Prelude.hashWithSalt` signingAlgorithmMnemonic
      `Prelude.hashWithSalt` signingAlgorithmType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData KeySigningKey where
  rnf KeySigningKey' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf dNSKEYRecord
      `Prelude.seq` Prelude.rnf dSRecord
      `Prelude.seq` Prelude.rnf digestAlgorithmMnemonic
      `Prelude.seq` Prelude.rnf digestAlgorithmType
      `Prelude.seq` Prelude.rnf digestValue
      `Prelude.seq` Prelude.rnf flag
      `Prelude.seq` Prelude.rnf keyTag
      `Prelude.seq` Prelude.rnf kmsArn
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf signingAlgorithmMnemonic
      `Prelude.seq` Prelude.rnf signingAlgorithmType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
