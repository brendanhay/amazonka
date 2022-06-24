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
-- Module      : Amazonka.Translate.Types.TerminologyProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TerminologyProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.EncryptionKey

-- | The properties of the custom terminology.
--
-- /See:/ 'newTerminologyProperties' smart constructor.
data TerminologyProperties = TerminologyProperties'
  { -- | The name of the custom terminology.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time at which the custom terminology was last update, based on the
    -- timestamp.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The number of terms included in the custom terminology.
    termCount :: Prelude.Maybe Prelude.Int,
    -- | The size of the file used when importing a custom terminology.
    sizeBytes :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the custom terminology.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The language codes for the target languages available with the custom
    -- terminology file. All possible target languages are returned in array.
    targetLanguageCodes :: Prelude.Maybe [Prelude.Text],
    -- | The description of the custom terminology properties.
    description :: Prelude.Maybe Prelude.Text,
    -- | The language code for the source text of the translation request for
    -- which the custom terminology is being used.
    sourceLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | The encryption key for the custom terminology.
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | The time at which the custom terminology was created, based on the
    -- timestamp.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminologyProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'terminologyProperties_name' - The name of the custom terminology.
--
-- 'lastUpdatedAt', 'terminologyProperties_lastUpdatedAt' - The time at which the custom terminology was last update, based on the
-- timestamp.
--
-- 'termCount', 'terminologyProperties_termCount' - The number of terms included in the custom terminology.
--
-- 'sizeBytes', 'terminologyProperties_sizeBytes' - The size of the file used when importing a custom terminology.
--
-- 'arn', 'terminologyProperties_arn' - The Amazon Resource Name (ARN) of the custom terminology.
--
-- 'targetLanguageCodes', 'terminologyProperties_targetLanguageCodes' - The language codes for the target languages available with the custom
-- terminology file. All possible target languages are returned in array.
--
-- 'description', 'terminologyProperties_description' - The description of the custom terminology properties.
--
-- 'sourceLanguageCode', 'terminologyProperties_sourceLanguageCode' - The language code for the source text of the translation request for
-- which the custom terminology is being used.
--
-- 'encryptionKey', 'terminologyProperties_encryptionKey' - The encryption key for the custom terminology.
--
-- 'createdAt', 'terminologyProperties_createdAt' - The time at which the custom terminology was created, based on the
-- timestamp.
newTerminologyProperties ::
  TerminologyProperties
newTerminologyProperties =
  TerminologyProperties'
    { name = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      termCount = Prelude.Nothing,
      sizeBytes = Prelude.Nothing,
      arn = Prelude.Nothing,
      targetLanguageCodes = Prelude.Nothing,
      description = Prelude.Nothing,
      sourceLanguageCode = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The name of the custom terminology.
terminologyProperties_name :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.Text)
terminologyProperties_name = Lens.lens (\TerminologyProperties' {name} -> name) (\s@TerminologyProperties' {} a -> s {name = a} :: TerminologyProperties)

-- | The time at which the custom terminology was last update, based on the
-- timestamp.
terminologyProperties_lastUpdatedAt :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.UTCTime)
terminologyProperties_lastUpdatedAt = Lens.lens (\TerminologyProperties' {lastUpdatedAt} -> lastUpdatedAt) (\s@TerminologyProperties' {} a -> s {lastUpdatedAt = a} :: TerminologyProperties) Prelude.. Lens.mapping Core._Time

-- | The number of terms included in the custom terminology.
terminologyProperties_termCount :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.Int)
terminologyProperties_termCount = Lens.lens (\TerminologyProperties' {termCount} -> termCount) (\s@TerminologyProperties' {} a -> s {termCount = a} :: TerminologyProperties)

-- | The size of the file used when importing a custom terminology.
terminologyProperties_sizeBytes :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.Int)
terminologyProperties_sizeBytes = Lens.lens (\TerminologyProperties' {sizeBytes} -> sizeBytes) (\s@TerminologyProperties' {} a -> s {sizeBytes = a} :: TerminologyProperties)

-- | The Amazon Resource Name (ARN) of the custom terminology.
terminologyProperties_arn :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.Text)
terminologyProperties_arn = Lens.lens (\TerminologyProperties' {arn} -> arn) (\s@TerminologyProperties' {} a -> s {arn = a} :: TerminologyProperties)

-- | The language codes for the target languages available with the custom
-- terminology file. All possible target languages are returned in array.
terminologyProperties_targetLanguageCodes :: Lens.Lens' TerminologyProperties (Prelude.Maybe [Prelude.Text])
terminologyProperties_targetLanguageCodes = Lens.lens (\TerminologyProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@TerminologyProperties' {} a -> s {targetLanguageCodes = a} :: TerminologyProperties) Prelude.. Lens.mapping Lens.coerced

-- | The description of the custom terminology properties.
terminologyProperties_description :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.Text)
terminologyProperties_description = Lens.lens (\TerminologyProperties' {description} -> description) (\s@TerminologyProperties' {} a -> s {description = a} :: TerminologyProperties)

-- | The language code for the source text of the translation request for
-- which the custom terminology is being used.
terminologyProperties_sourceLanguageCode :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.Text)
terminologyProperties_sourceLanguageCode = Lens.lens (\TerminologyProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@TerminologyProperties' {} a -> s {sourceLanguageCode = a} :: TerminologyProperties)

-- | The encryption key for the custom terminology.
terminologyProperties_encryptionKey :: Lens.Lens' TerminologyProperties (Prelude.Maybe EncryptionKey)
terminologyProperties_encryptionKey = Lens.lens (\TerminologyProperties' {encryptionKey} -> encryptionKey) (\s@TerminologyProperties' {} a -> s {encryptionKey = a} :: TerminologyProperties)

-- | The time at which the custom terminology was created, based on the
-- timestamp.
terminologyProperties_createdAt :: Lens.Lens' TerminologyProperties (Prelude.Maybe Prelude.UTCTime)
terminologyProperties_createdAt = Lens.lens (\TerminologyProperties' {createdAt} -> createdAt) (\s@TerminologyProperties' {} a -> s {createdAt = a} :: TerminologyProperties) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TerminologyProperties where
  parseJSON =
    Core.withObject
      "TerminologyProperties"
      ( \x ->
          TerminologyProperties'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastUpdatedAt")
            Prelude.<*> (x Core..:? "TermCount")
            Prelude.<*> (x Core..:? "SizeBytes")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> ( x Core..:? "TargetLanguageCodes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SourceLanguageCode")
            Prelude.<*> (x Core..:? "EncryptionKey")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable TerminologyProperties where
  hashWithSalt _salt TerminologyProperties' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` termCount
      `Prelude.hashWithSalt` sizeBytes
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` targetLanguageCodes
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData TerminologyProperties where
  rnf TerminologyProperties' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf termCount
      `Prelude.seq` Prelude.rnf sizeBytes
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf targetLanguageCodes
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf createdAt
