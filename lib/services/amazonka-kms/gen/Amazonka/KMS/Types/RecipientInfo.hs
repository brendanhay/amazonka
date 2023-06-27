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
-- Module      : Amazonka.KMS.Types.RecipientInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.RecipientInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.KeyEncryptionMechanism
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the party that receives the response from the
-- API operation.
--
-- This data type is designed to support Amazon Web Services Nitro
-- Enclaves, which lets you create an isolated compute environment in
-- Amazon EC2. For information about the interaction between KMS and Amazon
-- Web Services Nitro Enclaves, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves uses KMS>
-- in the /Key Management Service Developer Guide/.
--
-- /See:/ 'newRecipientInfo' smart constructor.
data RecipientInfo = RecipientInfo'
  { -- | The attestation document for an Amazon Web Services Nitro Enclave. This
    -- document includes the enclave\'s public key.
    attestationDocument :: Prelude.Maybe Data.Base64,
    -- | The encryption algorithm that KMS should use with the public key for an
    -- Amazon Web Services Nitro Enclave to encrypt plaintext values for the
    -- response. The only valid value is @RSAES_OAEP_SHA_256@.
    keyEncryptionAlgorithm :: Prelude.Maybe KeyEncryptionMechanism
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipientInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attestationDocument', 'recipientInfo_attestationDocument' - The attestation document for an Amazon Web Services Nitro Enclave. This
-- document includes the enclave\'s public key.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyEncryptionAlgorithm', 'recipientInfo_keyEncryptionAlgorithm' - The encryption algorithm that KMS should use with the public key for an
-- Amazon Web Services Nitro Enclave to encrypt plaintext values for the
-- response. The only valid value is @RSAES_OAEP_SHA_256@.
newRecipientInfo ::
  RecipientInfo
newRecipientInfo =
  RecipientInfo'
    { attestationDocument =
        Prelude.Nothing,
      keyEncryptionAlgorithm = Prelude.Nothing
    }

-- | The attestation document for an Amazon Web Services Nitro Enclave. This
-- document includes the enclave\'s public key.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
recipientInfo_attestationDocument :: Lens.Lens' RecipientInfo (Prelude.Maybe Prelude.ByteString)
recipientInfo_attestationDocument = Lens.lens (\RecipientInfo' {attestationDocument} -> attestationDocument) (\s@RecipientInfo' {} a -> s {attestationDocument = a} :: RecipientInfo) Prelude.. Lens.mapping Data._Base64

-- | The encryption algorithm that KMS should use with the public key for an
-- Amazon Web Services Nitro Enclave to encrypt plaintext values for the
-- response. The only valid value is @RSAES_OAEP_SHA_256@.
recipientInfo_keyEncryptionAlgorithm :: Lens.Lens' RecipientInfo (Prelude.Maybe KeyEncryptionMechanism)
recipientInfo_keyEncryptionAlgorithm = Lens.lens (\RecipientInfo' {keyEncryptionAlgorithm} -> keyEncryptionAlgorithm) (\s@RecipientInfo' {} a -> s {keyEncryptionAlgorithm = a} :: RecipientInfo)

instance Prelude.Hashable RecipientInfo where
  hashWithSalt _salt RecipientInfo' {..} =
    _salt
      `Prelude.hashWithSalt` attestationDocument
      `Prelude.hashWithSalt` keyEncryptionAlgorithm

instance Prelude.NFData RecipientInfo where
  rnf RecipientInfo' {..} =
    Prelude.rnf attestationDocument
      `Prelude.seq` Prelude.rnf keyEncryptionAlgorithm

instance Data.ToJSON RecipientInfo where
  toJSON RecipientInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttestationDocument" Data..=)
              Prelude.<$> attestationDocument,
            ("KeyEncryptionAlgorithm" Data..=)
              Prelude.<$> keyEncryptionAlgorithm
          ]
      )
