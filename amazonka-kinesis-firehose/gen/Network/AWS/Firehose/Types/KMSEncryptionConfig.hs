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
-- Module      : Network.AWS.Firehose.Types.KMSEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KMSEncryptionConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an encryption key for a destination in Amazon S3.
--
-- /See:/ 'newKMSEncryptionConfig' smart constructor.
data KMSEncryptionConfig = KMSEncryptionConfig'
  { -- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the
    -- same AWS Region as the destination Amazon S3 bucket. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    aWSKMSKeyARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KMSEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aWSKMSKeyARN', 'kmsEncryptionConfig_aWSKMSKeyARN' - The Amazon Resource Name (ARN) of the encryption key. Must belong to the
-- same AWS Region as the destination Amazon S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
newKMSEncryptionConfig ::
  -- | 'aWSKMSKeyARN'
  Core.Text ->
  KMSEncryptionConfig
newKMSEncryptionConfig pAWSKMSKeyARN_ =
  KMSEncryptionConfig' {aWSKMSKeyARN = pAWSKMSKeyARN_}

-- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the
-- same AWS Region as the destination Amazon S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
kmsEncryptionConfig_aWSKMSKeyARN :: Lens.Lens' KMSEncryptionConfig Core.Text
kmsEncryptionConfig_aWSKMSKeyARN = Lens.lens (\KMSEncryptionConfig' {aWSKMSKeyARN} -> aWSKMSKeyARN) (\s@KMSEncryptionConfig' {} a -> s {aWSKMSKeyARN = a} :: KMSEncryptionConfig)

instance Core.FromJSON KMSEncryptionConfig where
  parseJSON =
    Core.withObject
      "KMSEncryptionConfig"
      ( \x ->
          KMSEncryptionConfig'
            Core.<$> (x Core..: "AWSKMSKeyARN")
      )

instance Core.Hashable KMSEncryptionConfig

instance Core.NFData KMSEncryptionConfig

instance Core.ToJSON KMSEncryptionConfig where
  toJSON KMSEncryptionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AWSKMSKeyARN" Core..= aWSKMSKeyARN)]
      )
