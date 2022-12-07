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
-- Module      : Amazonka.Firehose.Types.KMSEncryptionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.KMSEncryptionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an encryption key for a destination in Amazon S3.
--
-- /See:/ 'newKMSEncryptionConfig' smart constructor.
data KMSEncryptionConfig = KMSEncryptionConfig'
  { -- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the
    -- same AWS Region as the destination Amazon S3 bucket. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    aWSKMSKeyARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  KMSEncryptionConfig
newKMSEncryptionConfig pAWSKMSKeyARN_ =
  KMSEncryptionConfig' {aWSKMSKeyARN = pAWSKMSKeyARN_}

-- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the
-- same AWS Region as the destination Amazon S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
kmsEncryptionConfig_aWSKMSKeyARN :: Lens.Lens' KMSEncryptionConfig Prelude.Text
kmsEncryptionConfig_aWSKMSKeyARN = Lens.lens (\KMSEncryptionConfig' {aWSKMSKeyARN} -> aWSKMSKeyARN) (\s@KMSEncryptionConfig' {} a -> s {aWSKMSKeyARN = a} :: KMSEncryptionConfig)

instance Data.FromJSON KMSEncryptionConfig where
  parseJSON =
    Data.withObject
      "KMSEncryptionConfig"
      ( \x ->
          KMSEncryptionConfig'
            Prelude.<$> (x Data..: "AWSKMSKeyARN")
      )

instance Prelude.Hashable KMSEncryptionConfig where
  hashWithSalt _salt KMSEncryptionConfig' {..} =
    _salt `Prelude.hashWithSalt` aWSKMSKeyARN

instance Prelude.NFData KMSEncryptionConfig where
  rnf KMSEncryptionConfig' {..} =
    Prelude.rnf aWSKMSKeyARN

instance Data.ToJSON KMSEncryptionConfig where
  toJSON KMSEncryptionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AWSKMSKeyARN" Data..= aWSKMSKeyARN)]
      )
