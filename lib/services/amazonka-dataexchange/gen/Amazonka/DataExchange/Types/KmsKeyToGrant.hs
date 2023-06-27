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
-- Module      : Amazonka.DataExchange.Types.KmsKeyToGrant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.KmsKeyToGrant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Resource Name (ARN) of the AWS KMS key used to encrypt the
-- shared S3 objects.
--
-- /See:/ 'newKmsKeyToGrant' smart constructor.
data KmsKeyToGrant = KmsKeyToGrant'
  { -- | The AWS KMS CMK (Key Management System Customer Managed Key) used to
    -- encrypt S3 objects in the shared S3 Bucket. AWS Data exchange will
    -- create a KMS grant for each subscriber to allow them to access and
    -- decrypt their entitled data that is encrypted using this KMS key
    -- specified.
    kmsKeyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KmsKeyToGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'kmsKeyToGrant_kmsKeyArn' - The AWS KMS CMK (Key Management System Customer Managed Key) used to
-- encrypt S3 objects in the shared S3 Bucket. AWS Data exchange will
-- create a KMS grant for each subscriber to allow them to access and
-- decrypt their entitled data that is encrypted using this KMS key
-- specified.
newKmsKeyToGrant ::
  -- | 'kmsKeyArn'
  Prelude.Text ->
  KmsKeyToGrant
newKmsKeyToGrant pKmsKeyArn_ =
  KmsKeyToGrant' {kmsKeyArn = pKmsKeyArn_}

-- | The AWS KMS CMK (Key Management System Customer Managed Key) used to
-- encrypt S3 objects in the shared S3 Bucket. AWS Data exchange will
-- create a KMS grant for each subscriber to allow them to access and
-- decrypt their entitled data that is encrypted using this KMS key
-- specified.
kmsKeyToGrant_kmsKeyArn :: Lens.Lens' KmsKeyToGrant Prelude.Text
kmsKeyToGrant_kmsKeyArn = Lens.lens (\KmsKeyToGrant' {kmsKeyArn} -> kmsKeyArn) (\s@KmsKeyToGrant' {} a -> s {kmsKeyArn = a} :: KmsKeyToGrant)

instance Data.FromJSON KmsKeyToGrant where
  parseJSON =
    Data.withObject
      "KmsKeyToGrant"
      ( \x ->
          KmsKeyToGrant' Prelude.<$> (x Data..: "KmsKeyArn")
      )

instance Prelude.Hashable KmsKeyToGrant where
  hashWithSalt _salt KmsKeyToGrant' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData KmsKeyToGrant where
  rnf KmsKeyToGrant' {..} = Prelude.rnf kmsKeyArn

instance Data.ToJSON KmsKeyToGrant where
  toJSON KmsKeyToGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KmsKeyArn" Data..= kmsKeyArn)]
      )
