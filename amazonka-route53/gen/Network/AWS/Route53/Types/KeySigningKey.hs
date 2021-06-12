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
-- Module      : Network.AWS.Route53.Types.KeySigningKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.KeySigningKey where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal

-- | A key-signing key (KSK) is a complex type that represents a
-- public\/private key pair. The private key is used to generate a digital
-- signature for the zone signing key (ZSK). The public key is stored in
-- the DNS and is used to authenticate the ZSK. A KSK is always associated
-- with a hosted zone; it cannot exist by itself.
--
-- /See:/ 'newKeySigningKey' smart constructor.
data KeySigningKey = KeySigningKey'
  { -- | An integer used to represent the delegation signer digest algorithm.
    -- This value must follow the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
    digestAlgorithmType :: Core.Maybe Core.Int,
    -- | The last time that the key-signing key (KSK) was changed.
    lastModifiedDate :: Core.Maybe Core.ISO8601,
    -- | The status message provided for the following key-signing key (KSK)
    -- statuses: @ACTION_NEEDED@ or @INTERNAL_FAILURE@. The status message
    -- includes information about what the problem might be and steps that you
    -- can take to correct the issue.
    statusMessage :: Core.Maybe Core.Text,
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
    --     resolve. For example, the customer managed customer master key (CMK)
    --     might have been deleted, or the permissions for the customer managed
    --     CMK might have been changed.
    --
    -- [INTERNAL_FAILURE]
    --     There was an error during a request. Before you can continue to work
    --     with DNSSEC signing, including actions that involve this KSK, you
    --     must correct the problem. For example, you may need to activate or
    --     deactivate the KSK.
    status :: Core.Maybe Core.Text,
    -- | The date when the key-signing key (KSK) was created.
    createdDate :: Core.Maybe Core.ISO8601,
    -- | A string used to represent the signing algorithm. This value must follow
    -- the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
    signingAlgorithmMnemonic :: Core.Maybe Core.Text,
    -- | The public key, represented as a Base64 encoding, as required by
    -- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Page 5>.
    publicKey :: Core.Maybe Core.Text,
    -- | A string that represents a DNSKEY record.
    dNSKEYRecord :: Core.Maybe Core.Text,
    -- | A cryptographic digest of a DNSKEY resource record (RR). DNSKEY records
    -- are used to publish the public key that resolvers can use to verify
    -- DNSSEC signatures that are used to secure certain kinds of information
    -- provided by the DNS system.
    digestValue :: Core.Maybe Core.Text,
    -- | A string used to represent the delegation signer digest algorithm. This
    -- value must follow the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
    digestAlgorithmMnemonic :: Core.Maybe Core.Text,
    -- | A string used to identify a key-signing key (KSK). @Name@ can include
    -- numbers, letters, and underscores (_). @Name@ must be unique for each
    -- key-signing key in the same hosted zone.
    name :: Core.Maybe Core.Text,
    -- | An integer used to represent the signing algorithm. This value must
    -- follow the guidelines provided by
    -- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
    signingAlgorithmType :: Core.Maybe Core.Int,
    -- | An integer that specifies how the key is used. For key-signing key
    -- (KSK), this value is always 257.
    flag :: Core.Maybe Core.Int,
    -- | The Amazon resource name (ARN) used to identify the customer managed
    -- customer master key (CMK) in AWS Key Management Service (AWS KMS). The
    -- @KmsArn@ must be unique for each key-signing key (KSK) in a single
    -- hosted zone.
    --
    -- You must configure the CMK as follows:
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
    --     -   @\"Service\": \"api-service.dnssec.route53.aws.internal\"@
    --
    -- For more information about working with the customer managed CMK in AWS
    -- KMS, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service concepts>.
    kmsArn :: Core.Maybe Core.Text,
    -- | An integer used to identify the DNSSEC record for the domain name. The
    -- process used to calculate the value is described in
    -- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Appendix B>.
    keyTag :: Core.Maybe Core.Natural,
    -- | A string that represents a delegation signer (DS) record.
    dSRecord :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KeySigningKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'digestAlgorithmType', 'keySigningKey_digestAlgorithmType' - An integer used to represent the delegation signer digest algorithm.
-- This value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
--
-- 'lastModifiedDate', 'keySigningKey_lastModifiedDate' - The last time that the key-signing key (KSK) was changed.
--
-- 'statusMessage', 'keySigningKey_statusMessage' - The status message provided for the following key-signing key (KSK)
-- statuses: @ACTION_NEEDED@ or @INTERNAL_FAILURE@. The status message
-- includes information about what the problem might be and steps that you
-- can take to correct the issue.
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
--     resolve. For example, the customer managed customer master key (CMK)
--     might have been deleted, or the permissions for the customer managed
--     CMK might have been changed.
--
-- [INTERNAL_FAILURE]
--     There was an error during a request. Before you can continue to work
--     with DNSSEC signing, including actions that involve this KSK, you
--     must correct the problem. For example, you may need to activate or
--     deactivate the KSK.
--
-- 'createdDate', 'keySigningKey_createdDate' - The date when the key-signing key (KSK) was created.
--
-- 'signingAlgorithmMnemonic', 'keySigningKey_signingAlgorithmMnemonic' - A string used to represent the signing algorithm. This value must follow
-- the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
--
-- 'publicKey', 'keySigningKey_publicKey' - The public key, represented as a Base64 encoding, as required by
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Page 5>.
--
-- 'dNSKEYRecord', 'keySigningKey_dNSKEYRecord' - A string that represents a DNSKEY record.
--
-- 'digestValue', 'keySigningKey_digestValue' - A cryptographic digest of a DNSKEY resource record (RR). DNSKEY records
-- are used to publish the public key that resolvers can use to verify
-- DNSSEC signatures that are used to secure certain kinds of information
-- provided by the DNS system.
--
-- 'digestAlgorithmMnemonic', 'keySigningKey_digestAlgorithmMnemonic' - A string used to represent the delegation signer digest algorithm. This
-- value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
--
-- 'name', 'keySigningKey_name' - A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
--
-- 'signingAlgorithmType', 'keySigningKey_signingAlgorithmType' - An integer used to represent the signing algorithm. This value must
-- follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
--
-- 'flag', 'keySigningKey_flag' - An integer that specifies how the key is used. For key-signing key
-- (KSK), this value is always 257.
--
-- 'kmsArn', 'keySigningKey_kmsArn' - The Amazon resource name (ARN) used to identify the customer managed
-- customer master key (CMK) in AWS Key Management Service (AWS KMS). The
-- @KmsArn@ must be unique for each key-signing key (KSK) in a single
-- hosted zone.
--
-- You must configure the CMK as follows:
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
--     -   @\"Service\": \"api-service.dnssec.route53.aws.internal\"@
--
-- For more information about working with the customer managed CMK in AWS
-- KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service concepts>.
--
-- 'keyTag', 'keySigningKey_keyTag' - An integer used to identify the DNSSEC record for the domain name. The
-- process used to calculate the value is described in
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Appendix B>.
--
-- 'dSRecord', 'keySigningKey_dSRecord' - A string that represents a delegation signer (DS) record.
newKeySigningKey ::
  KeySigningKey
newKeySigningKey =
  KeySigningKey'
    { digestAlgorithmType = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      statusMessage = Core.Nothing,
      status = Core.Nothing,
      createdDate = Core.Nothing,
      signingAlgorithmMnemonic = Core.Nothing,
      publicKey = Core.Nothing,
      dNSKEYRecord = Core.Nothing,
      digestValue = Core.Nothing,
      digestAlgorithmMnemonic = Core.Nothing,
      name = Core.Nothing,
      signingAlgorithmType = Core.Nothing,
      flag = Core.Nothing,
      kmsArn = Core.Nothing,
      keyTag = Core.Nothing,
      dSRecord = Core.Nothing
    }

-- | An integer used to represent the delegation signer digest algorithm.
-- This value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
keySigningKey_digestAlgorithmType :: Lens.Lens' KeySigningKey (Core.Maybe Core.Int)
keySigningKey_digestAlgorithmType = Lens.lens (\KeySigningKey' {digestAlgorithmType} -> digestAlgorithmType) (\s@KeySigningKey' {} a -> s {digestAlgorithmType = a} :: KeySigningKey)

-- | The last time that the key-signing key (KSK) was changed.
keySigningKey_lastModifiedDate :: Lens.Lens' KeySigningKey (Core.Maybe Core.UTCTime)
keySigningKey_lastModifiedDate = Lens.lens (\KeySigningKey' {lastModifiedDate} -> lastModifiedDate) (\s@KeySigningKey' {} a -> s {lastModifiedDate = a} :: KeySigningKey) Core.. Lens.mapping Core._Time

-- | The status message provided for the following key-signing key (KSK)
-- statuses: @ACTION_NEEDED@ or @INTERNAL_FAILURE@. The status message
-- includes information about what the problem might be and steps that you
-- can take to correct the issue.
keySigningKey_statusMessage :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_statusMessage = Lens.lens (\KeySigningKey' {statusMessage} -> statusMessage) (\s@KeySigningKey' {} a -> s {statusMessage = a} :: KeySigningKey)

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
--     resolve. For example, the customer managed customer master key (CMK)
--     might have been deleted, or the permissions for the customer managed
--     CMK might have been changed.
--
-- [INTERNAL_FAILURE]
--     There was an error during a request. Before you can continue to work
--     with DNSSEC signing, including actions that involve this KSK, you
--     must correct the problem. For example, you may need to activate or
--     deactivate the KSK.
keySigningKey_status :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_status = Lens.lens (\KeySigningKey' {status} -> status) (\s@KeySigningKey' {} a -> s {status = a} :: KeySigningKey)

-- | The date when the key-signing key (KSK) was created.
keySigningKey_createdDate :: Lens.Lens' KeySigningKey (Core.Maybe Core.UTCTime)
keySigningKey_createdDate = Lens.lens (\KeySigningKey' {createdDate} -> createdDate) (\s@KeySigningKey' {} a -> s {createdDate = a} :: KeySigningKey) Core.. Lens.mapping Core._Time

-- | A string used to represent the signing algorithm. This value must follow
-- the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
keySigningKey_signingAlgorithmMnemonic :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_signingAlgorithmMnemonic = Lens.lens (\KeySigningKey' {signingAlgorithmMnemonic} -> signingAlgorithmMnemonic) (\s@KeySigningKey' {} a -> s {signingAlgorithmMnemonic = a} :: KeySigningKey)

-- | The public key, represented as a Base64 encoding, as required by
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Page 5>.
keySigningKey_publicKey :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_publicKey = Lens.lens (\KeySigningKey' {publicKey} -> publicKey) (\s@KeySigningKey' {} a -> s {publicKey = a} :: KeySigningKey)

-- | A string that represents a DNSKEY record.
keySigningKey_dNSKEYRecord :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_dNSKEYRecord = Lens.lens (\KeySigningKey' {dNSKEYRecord} -> dNSKEYRecord) (\s@KeySigningKey' {} a -> s {dNSKEYRecord = a} :: KeySigningKey)

-- | A cryptographic digest of a DNSKEY resource record (RR). DNSKEY records
-- are used to publish the public key that resolvers can use to verify
-- DNSSEC signatures that are used to secure certain kinds of information
-- provided by the DNS system.
keySigningKey_digestValue :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_digestValue = Lens.lens (\KeySigningKey' {digestValue} -> digestValue) (\s@KeySigningKey' {} a -> s {digestValue = a} :: KeySigningKey)

-- | A string used to represent the delegation signer digest algorithm. This
-- value must follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.3 RFC-8624 Section 3.3>.
keySigningKey_digestAlgorithmMnemonic :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_digestAlgorithmMnemonic = Lens.lens (\KeySigningKey' {digestAlgorithmMnemonic} -> digestAlgorithmMnemonic) (\s@KeySigningKey' {} a -> s {digestAlgorithmMnemonic = a} :: KeySigningKey)

-- | A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
keySigningKey_name :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_name = Lens.lens (\KeySigningKey' {name} -> name) (\s@KeySigningKey' {} a -> s {name = a} :: KeySigningKey)

-- | An integer used to represent the signing algorithm. This value must
-- follow the guidelines provided by
-- <https://tools.ietf.org/html/rfc8624#section-3.1 RFC-8624 Section 3.1>.
keySigningKey_signingAlgorithmType :: Lens.Lens' KeySigningKey (Core.Maybe Core.Int)
keySigningKey_signingAlgorithmType = Lens.lens (\KeySigningKey' {signingAlgorithmType} -> signingAlgorithmType) (\s@KeySigningKey' {} a -> s {signingAlgorithmType = a} :: KeySigningKey)

-- | An integer that specifies how the key is used. For key-signing key
-- (KSK), this value is always 257.
keySigningKey_flag :: Lens.Lens' KeySigningKey (Core.Maybe Core.Int)
keySigningKey_flag = Lens.lens (\KeySigningKey' {flag} -> flag) (\s@KeySigningKey' {} a -> s {flag = a} :: KeySigningKey)

-- | The Amazon resource name (ARN) used to identify the customer managed
-- customer master key (CMK) in AWS Key Management Service (AWS KMS). The
-- @KmsArn@ must be unique for each key-signing key (KSK) in a single
-- hosted zone.
--
-- You must configure the CMK as follows:
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
--     -   @\"Service\": \"api-service.dnssec.route53.aws.internal\"@
--
-- For more information about working with the customer managed CMK in AWS
-- KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service concepts>.
keySigningKey_kmsArn :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_kmsArn = Lens.lens (\KeySigningKey' {kmsArn} -> kmsArn) (\s@KeySigningKey' {} a -> s {kmsArn = a} :: KeySigningKey)

-- | An integer used to identify the DNSSEC record for the domain name. The
-- process used to calculate the value is described in
-- <https://tools.ietf.org/rfc/rfc4034.txt RFC-4034 Appendix B>.
keySigningKey_keyTag :: Lens.Lens' KeySigningKey (Core.Maybe Core.Natural)
keySigningKey_keyTag = Lens.lens (\KeySigningKey' {keyTag} -> keyTag) (\s@KeySigningKey' {} a -> s {keyTag = a} :: KeySigningKey)

-- | A string that represents a delegation signer (DS) record.
keySigningKey_dSRecord :: Lens.Lens' KeySigningKey (Core.Maybe Core.Text)
keySigningKey_dSRecord = Lens.lens (\KeySigningKey' {dSRecord} -> dSRecord) (\s@KeySigningKey' {} a -> s {dSRecord = a} :: KeySigningKey)

instance Core.FromXML KeySigningKey where
  parseXML x =
    KeySigningKey'
      Core.<$> (x Core..@? "DigestAlgorithmType")
      Core.<*> (x Core..@? "LastModifiedDate")
      Core.<*> (x Core..@? "StatusMessage")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "CreatedDate")
      Core.<*> (x Core..@? "SigningAlgorithmMnemonic")
      Core.<*> (x Core..@? "PublicKey")
      Core.<*> (x Core..@? "DNSKEYRecord")
      Core.<*> (x Core..@? "DigestValue")
      Core.<*> (x Core..@? "DigestAlgorithmMnemonic")
      Core.<*> (x Core..@? "Name")
      Core.<*> (x Core..@? "SigningAlgorithmType")
      Core.<*> (x Core..@? "Flag")
      Core.<*> (x Core..@? "KmsArn")
      Core.<*> (x Core..@? "KeyTag")
      Core.<*> (x Core..@? "DSRecord")

instance Core.Hashable KeySigningKey

instance Core.NFData KeySigningKey
