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
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CloudWatchEncryption where

import Network.AWS.Glue.Types.CloudWatchEncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies how Amazon CloudWatch data should be encrypted.
--
-- /See:/ 'newCloudWatchEncryption' smart constructor.
data CloudWatchEncryption = CloudWatchEncryption'
  { -- | The encryption mode to use for CloudWatch data.
    cloudWatchEncryptionMode :: Prelude.Maybe CloudWatchEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
    -- data.
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchEncryptionMode', 'cloudWatchEncryption_cloudWatchEncryptionMode' - The encryption mode to use for CloudWatch data.
--
-- 'kmsKeyArn', 'cloudWatchEncryption_kmsKeyArn' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
newCloudWatchEncryption ::
  CloudWatchEncryption
newCloudWatchEncryption =
  CloudWatchEncryption'
    { cloudWatchEncryptionMode =
        Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | The encryption mode to use for CloudWatch data.
cloudWatchEncryption_cloudWatchEncryptionMode :: Lens.Lens' CloudWatchEncryption (Prelude.Maybe CloudWatchEncryptionMode)
cloudWatchEncryption_cloudWatchEncryptionMode = Lens.lens (\CloudWatchEncryption' {cloudWatchEncryptionMode} -> cloudWatchEncryptionMode) (\s@CloudWatchEncryption' {} a -> s {cloudWatchEncryptionMode = a} :: CloudWatchEncryption)

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
cloudWatchEncryption_kmsKeyArn :: Lens.Lens' CloudWatchEncryption (Prelude.Maybe Prelude.Text)
cloudWatchEncryption_kmsKeyArn = Lens.lens (\CloudWatchEncryption' {kmsKeyArn} -> kmsKeyArn) (\s@CloudWatchEncryption' {} a -> s {kmsKeyArn = a} :: CloudWatchEncryption)

instance Prelude.FromJSON CloudWatchEncryption where
  parseJSON =
    Prelude.withObject
      "CloudWatchEncryption"
      ( \x ->
          CloudWatchEncryption'
            Prelude.<$> (x Prelude..:? "CloudWatchEncryptionMode")
            Prelude.<*> (x Prelude..:? "KmsKeyArn")
      )

instance Prelude.Hashable CloudWatchEncryption

instance Prelude.NFData CloudWatchEncryption

instance Prelude.ToJSON CloudWatchEncryption where
  toJSON CloudWatchEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CloudWatchEncryptionMode" Prelude..=)
              Prelude.<$> cloudWatchEncryptionMode,
            ("KmsKeyArn" Prelude..=) Prelude.<$> kmsKeyArn
          ]
      )
