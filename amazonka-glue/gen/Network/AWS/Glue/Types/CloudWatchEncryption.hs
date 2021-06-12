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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CloudWatchEncryptionMode
import qualified Network.AWS.Lens as Lens

-- | Specifies how Amazon CloudWatch data should be encrypted.
--
-- /See:/ 'newCloudWatchEncryption' smart constructor.
data CloudWatchEncryption = CloudWatchEncryption'
  { -- | The encryption mode to use for CloudWatch data.
    cloudWatchEncryptionMode :: Core.Maybe CloudWatchEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
    -- data.
    kmsKeyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | The encryption mode to use for CloudWatch data.
cloudWatchEncryption_cloudWatchEncryptionMode :: Lens.Lens' CloudWatchEncryption (Core.Maybe CloudWatchEncryptionMode)
cloudWatchEncryption_cloudWatchEncryptionMode = Lens.lens (\CloudWatchEncryption' {cloudWatchEncryptionMode} -> cloudWatchEncryptionMode) (\s@CloudWatchEncryption' {} a -> s {cloudWatchEncryptionMode = a} :: CloudWatchEncryption)

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
cloudWatchEncryption_kmsKeyArn :: Lens.Lens' CloudWatchEncryption (Core.Maybe Core.Text)
cloudWatchEncryption_kmsKeyArn = Lens.lens (\CloudWatchEncryption' {kmsKeyArn} -> kmsKeyArn) (\s@CloudWatchEncryption' {} a -> s {kmsKeyArn = a} :: CloudWatchEncryption)

instance Core.FromJSON CloudWatchEncryption where
  parseJSON =
    Core.withObject
      "CloudWatchEncryption"
      ( \x ->
          CloudWatchEncryption'
            Core.<$> (x Core..:? "CloudWatchEncryptionMode")
            Core.<*> (x Core..:? "KmsKeyArn")
      )

instance Core.Hashable CloudWatchEncryption

instance Core.NFData CloudWatchEncryption

instance Core.ToJSON CloudWatchEncryption where
  toJSON CloudWatchEncryption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchEncryptionMode" Core..=)
              Core.<$> cloudWatchEncryptionMode,
            ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )
