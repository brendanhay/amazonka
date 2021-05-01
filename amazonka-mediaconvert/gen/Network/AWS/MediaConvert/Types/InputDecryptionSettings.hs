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
-- Module      : Network.AWS.MediaConvert.Types.InputDecryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputDecryptionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DecryptionMode
import qualified Network.AWS.Prelude as Prelude

-- | Settings for decrypting any input files that you encrypt before you
-- upload them to Amazon S3. MediaConvert can decrypt files only when you
-- use AWS Key Management Service (KMS) to encrypt the data key that you
-- use to encrypt your content.
--
-- /See:/ 'newInputDecryptionSettings' smart constructor.
data InputDecryptionSettings = InputDecryptionSettings'
  { -- | Specify the encryption mode that you used to encrypt your input files.
    decryptionMode :: Prelude.Maybe DecryptionMode,
    -- | Warning! Don\'t provide your encryption key in plaintext. Your job
    -- settings could be intercepted, making your encrypted content vulnerable.
    -- Specify the encrypted version of the data key that you used to encrypt
    -- your content. The data key must be encrypted by AWS Key Management
    -- Service (KMS). The key can be 128, 192, or 256 bits.
    encryptedDecryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Specify the initialization vector that you used when you encrypted your
    -- content before uploading it to Amazon S3. You can use a 16-byte
    -- initialization vector with any encryption mode. Or, you can use a
    -- 12-byte initialization vector with GCM or CTR. MediaConvert accepts only
    -- initialization vectors that are base64-encoded.
    initializationVector :: Prelude.Maybe Prelude.Text,
    -- | Specify the AWS Region for AWS Key Management Service (KMS) that you
    -- used to encrypt your data key, if that Region is different from the one
    -- you are using for AWS Elemental MediaConvert.
    kmsKeyRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputDecryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decryptionMode', 'inputDecryptionSettings_decryptionMode' - Specify the encryption mode that you used to encrypt your input files.
--
-- 'encryptedDecryptionKey', 'inputDecryptionSettings_encryptedDecryptionKey' - Warning! Don\'t provide your encryption key in plaintext. Your job
-- settings could be intercepted, making your encrypted content vulnerable.
-- Specify the encrypted version of the data key that you used to encrypt
-- your content. The data key must be encrypted by AWS Key Management
-- Service (KMS). The key can be 128, 192, or 256 bits.
--
-- 'initializationVector', 'inputDecryptionSettings_initializationVector' - Specify the initialization vector that you used when you encrypted your
-- content before uploading it to Amazon S3. You can use a 16-byte
-- initialization vector with any encryption mode. Or, you can use a
-- 12-byte initialization vector with GCM or CTR. MediaConvert accepts only
-- initialization vectors that are base64-encoded.
--
-- 'kmsKeyRegion', 'inputDecryptionSettings_kmsKeyRegion' - Specify the AWS Region for AWS Key Management Service (KMS) that you
-- used to encrypt your data key, if that Region is different from the one
-- you are using for AWS Elemental MediaConvert.
newInputDecryptionSettings ::
  InputDecryptionSettings
newInputDecryptionSettings =
  InputDecryptionSettings'
    { decryptionMode =
        Prelude.Nothing,
      encryptedDecryptionKey = Prelude.Nothing,
      initializationVector = Prelude.Nothing,
      kmsKeyRegion = Prelude.Nothing
    }

-- | Specify the encryption mode that you used to encrypt your input files.
inputDecryptionSettings_decryptionMode :: Lens.Lens' InputDecryptionSettings (Prelude.Maybe DecryptionMode)
inputDecryptionSettings_decryptionMode = Lens.lens (\InputDecryptionSettings' {decryptionMode} -> decryptionMode) (\s@InputDecryptionSettings' {} a -> s {decryptionMode = a} :: InputDecryptionSettings)

-- | Warning! Don\'t provide your encryption key in plaintext. Your job
-- settings could be intercepted, making your encrypted content vulnerable.
-- Specify the encrypted version of the data key that you used to encrypt
-- your content. The data key must be encrypted by AWS Key Management
-- Service (KMS). The key can be 128, 192, or 256 bits.
inputDecryptionSettings_encryptedDecryptionKey :: Lens.Lens' InputDecryptionSettings (Prelude.Maybe Prelude.Text)
inputDecryptionSettings_encryptedDecryptionKey = Lens.lens (\InputDecryptionSettings' {encryptedDecryptionKey} -> encryptedDecryptionKey) (\s@InputDecryptionSettings' {} a -> s {encryptedDecryptionKey = a} :: InputDecryptionSettings)

-- | Specify the initialization vector that you used when you encrypted your
-- content before uploading it to Amazon S3. You can use a 16-byte
-- initialization vector with any encryption mode. Or, you can use a
-- 12-byte initialization vector with GCM or CTR. MediaConvert accepts only
-- initialization vectors that are base64-encoded.
inputDecryptionSettings_initializationVector :: Lens.Lens' InputDecryptionSettings (Prelude.Maybe Prelude.Text)
inputDecryptionSettings_initializationVector = Lens.lens (\InputDecryptionSettings' {initializationVector} -> initializationVector) (\s@InputDecryptionSettings' {} a -> s {initializationVector = a} :: InputDecryptionSettings)

-- | Specify the AWS Region for AWS Key Management Service (KMS) that you
-- used to encrypt your data key, if that Region is different from the one
-- you are using for AWS Elemental MediaConvert.
inputDecryptionSettings_kmsKeyRegion :: Lens.Lens' InputDecryptionSettings (Prelude.Maybe Prelude.Text)
inputDecryptionSettings_kmsKeyRegion = Lens.lens (\InputDecryptionSettings' {kmsKeyRegion} -> kmsKeyRegion) (\s@InputDecryptionSettings' {} a -> s {kmsKeyRegion = a} :: InputDecryptionSettings)

instance Prelude.FromJSON InputDecryptionSettings where
  parseJSON =
    Prelude.withObject
      "InputDecryptionSettings"
      ( \x ->
          InputDecryptionSettings'
            Prelude.<$> (x Prelude..:? "decryptionMode")
            Prelude.<*> (x Prelude..:? "encryptedDecryptionKey")
            Prelude.<*> (x Prelude..:? "initializationVector")
            Prelude.<*> (x Prelude..:? "kmsKeyRegion")
      )

instance Prelude.Hashable InputDecryptionSettings

instance Prelude.NFData InputDecryptionSettings

instance Prelude.ToJSON InputDecryptionSettings where
  toJSON InputDecryptionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("decryptionMode" Prelude..=)
              Prelude.<$> decryptionMode,
            ("encryptedDecryptionKey" Prelude..=)
              Prelude.<$> encryptedDecryptionKey,
            ("initializationVector" Prelude..=)
              Prelude.<$> initializationVector,
            ("kmsKeyRegion" Prelude..=)
              Prelude.<$> kmsKeyRegion
          ]
      )
