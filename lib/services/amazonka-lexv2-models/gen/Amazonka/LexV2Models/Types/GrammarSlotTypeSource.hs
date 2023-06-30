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
-- Module      : Amazonka.LexV2Models.Types.GrammarSlotTypeSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.GrammarSlotTypeSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon S3 bucket name and location for the grammar that is
-- the source for the slot type.
--
-- /See:/ 'newGrammarSlotTypeSource' smart constructor.
data GrammarSlotTypeSource = GrammarSlotTypeSource'
  { -- | The Amazon KMS key required to decrypt the contents of the grammar, if
    -- any.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket that contains the grammar source.
    s3BucketName :: Prelude.Text,
    -- | The path to the grammar in the S3 bucket.
    s3ObjectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrammarSlotTypeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'grammarSlotTypeSource_kmsKeyArn' - The Amazon KMS key required to decrypt the contents of the grammar, if
-- any.
--
-- 's3BucketName', 'grammarSlotTypeSource_s3BucketName' - The name of the S3 bucket that contains the grammar source.
--
-- 's3ObjectKey', 'grammarSlotTypeSource_s3ObjectKey' - The path to the grammar in the S3 bucket.
newGrammarSlotTypeSource ::
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3ObjectKey'
  Prelude.Text ->
  GrammarSlotTypeSource
newGrammarSlotTypeSource pS3BucketName_ pS3ObjectKey_ =
  GrammarSlotTypeSource'
    { kmsKeyArn = Prelude.Nothing,
      s3BucketName = pS3BucketName_,
      s3ObjectKey = pS3ObjectKey_
    }

-- | The Amazon KMS key required to decrypt the contents of the grammar, if
-- any.
grammarSlotTypeSource_kmsKeyArn :: Lens.Lens' GrammarSlotTypeSource (Prelude.Maybe Prelude.Text)
grammarSlotTypeSource_kmsKeyArn = Lens.lens (\GrammarSlotTypeSource' {kmsKeyArn} -> kmsKeyArn) (\s@GrammarSlotTypeSource' {} a -> s {kmsKeyArn = a} :: GrammarSlotTypeSource)

-- | The name of the S3 bucket that contains the grammar source.
grammarSlotTypeSource_s3BucketName :: Lens.Lens' GrammarSlotTypeSource Prelude.Text
grammarSlotTypeSource_s3BucketName = Lens.lens (\GrammarSlotTypeSource' {s3BucketName} -> s3BucketName) (\s@GrammarSlotTypeSource' {} a -> s {s3BucketName = a} :: GrammarSlotTypeSource)

-- | The path to the grammar in the S3 bucket.
grammarSlotTypeSource_s3ObjectKey :: Lens.Lens' GrammarSlotTypeSource Prelude.Text
grammarSlotTypeSource_s3ObjectKey = Lens.lens (\GrammarSlotTypeSource' {s3ObjectKey} -> s3ObjectKey) (\s@GrammarSlotTypeSource' {} a -> s {s3ObjectKey = a} :: GrammarSlotTypeSource)

instance Data.FromJSON GrammarSlotTypeSource where
  parseJSON =
    Data.withObject
      "GrammarSlotTypeSource"
      ( \x ->
          GrammarSlotTypeSource'
            Prelude.<$> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..: "s3BucketName")
            Prelude.<*> (x Data..: "s3ObjectKey")
      )

instance Prelude.Hashable GrammarSlotTypeSource where
  hashWithSalt _salt GrammarSlotTypeSource' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3ObjectKey

instance Prelude.NFData GrammarSlotTypeSource where
  rnf GrammarSlotTypeSource' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3ObjectKey

instance Data.ToJSON GrammarSlotTypeSource where
  toJSON GrammarSlotTypeSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("s3BucketName" Data..= s3BucketName),
            Prelude.Just ("s3ObjectKey" Data..= s3ObjectKey)
          ]
      )
