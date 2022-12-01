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
-- Module      : Amazonka.Inspector2.Types.Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details of the Amazon S3 bucket and KMS key used to export
-- findings.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | The prefix of the KMS key used to export findings.
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket to export findings to.
    bucketName :: Prelude.Text,
    -- | The ARN of the KMS key used to encrypt data when exporting findings.
    kmsKeyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'destination_keyPrefix' - The prefix of the KMS key used to export findings.
--
-- 'bucketName', 'destination_bucketName' - The name of the Amazon S3 bucket to export findings to.
--
-- 'kmsKeyArn', 'destination_kmsKeyArn' - The ARN of the KMS key used to encrypt data when exporting findings.
newDestination ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'kmsKeyArn'
  Prelude.Text ->
  Destination
newDestination pBucketName_ pKmsKeyArn_ =
  Destination'
    { keyPrefix = Prelude.Nothing,
      bucketName = pBucketName_,
      kmsKeyArn = pKmsKeyArn_
    }

-- | The prefix of the KMS key used to export findings.
destination_keyPrefix :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_keyPrefix = Lens.lens (\Destination' {keyPrefix} -> keyPrefix) (\s@Destination' {} a -> s {keyPrefix = a} :: Destination)

-- | The name of the Amazon S3 bucket to export findings to.
destination_bucketName :: Lens.Lens' Destination Prelude.Text
destination_bucketName = Lens.lens (\Destination' {bucketName} -> bucketName) (\s@Destination' {} a -> s {bucketName = a} :: Destination)

-- | The ARN of the KMS key used to encrypt data when exporting findings.
destination_kmsKeyArn :: Lens.Lens' Destination Prelude.Text
destination_kmsKeyArn = Lens.lens (\Destination' {kmsKeyArn} -> kmsKeyArn) (\s@Destination' {} a -> s {kmsKeyArn = a} :: Destination)

instance Core.FromJSON Destination where
  parseJSON =
    Core.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Core..:? "keyPrefix")
            Prelude.<*> (x Core..: "bucketName")
            Prelude.<*> (x Core..: "kmsKeyArn")
      )

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt `Prelude.hashWithSalt` keyPrefix
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf kmsKeyArn

instance Core.ToJSON Destination where
  toJSON Destination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Core..=) Prelude.<$> keyPrefix,
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just ("kmsKeyArn" Core..= kmsKeyArn)
          ]
      )
