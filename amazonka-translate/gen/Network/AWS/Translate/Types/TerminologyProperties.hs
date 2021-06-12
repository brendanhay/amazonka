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
-- Module      : Network.AWS.Translate.Types.TerminologyProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Translate.Types.EncryptionKey

-- | The properties of the custom terminology.
--
-- /See:/ 'newTerminologyProperties' smart constructor.
data TerminologyProperties = TerminologyProperties'
  { -- | The encryption key for the custom terminology.
    encryptionKey :: Core.Maybe EncryptionKey,
    -- | The Amazon Resource Name (ARN) of the custom terminology.
    arn :: Core.Maybe Core.Text,
    -- | The language codes for the target languages available with the custom
    -- terminology file. All possible target languages are returned in array.
    targetLanguageCodes :: Core.Maybe [Core.Text],
    -- | The time at which the custom terminology was created, based on the
    -- timestamp.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The name of the custom terminology.
    name :: Core.Maybe Core.Text,
    -- | The size of the file used when importing a custom terminology.
    sizeBytes :: Core.Maybe Core.Int,
    -- | The description of the custom terminology properties.
    description :: Core.Maybe Core.Text,
    -- | The number of terms included in the custom terminology.
    termCount :: Core.Maybe Core.Int,
    -- | The language code for the source text of the translation request for
    -- which the custom terminology is being used.
    sourceLanguageCode :: Core.Maybe Core.Text,
    -- | The time at which the custom terminology was last update, based on the
    -- timestamp.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminologyProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'terminologyProperties_encryptionKey' - The encryption key for the custom terminology.
--
-- 'arn', 'terminologyProperties_arn' - The Amazon Resource Name (ARN) of the custom terminology.
--
-- 'targetLanguageCodes', 'terminologyProperties_targetLanguageCodes' - The language codes for the target languages available with the custom
-- terminology file. All possible target languages are returned in array.
--
-- 'createdAt', 'terminologyProperties_createdAt' - The time at which the custom terminology was created, based on the
-- timestamp.
--
-- 'name', 'terminologyProperties_name' - The name of the custom terminology.
--
-- 'sizeBytes', 'terminologyProperties_sizeBytes' - The size of the file used when importing a custom terminology.
--
-- 'description', 'terminologyProperties_description' - The description of the custom terminology properties.
--
-- 'termCount', 'terminologyProperties_termCount' - The number of terms included in the custom terminology.
--
-- 'sourceLanguageCode', 'terminologyProperties_sourceLanguageCode' - The language code for the source text of the translation request for
-- which the custom terminology is being used.
--
-- 'lastUpdatedAt', 'terminologyProperties_lastUpdatedAt' - The time at which the custom terminology was last update, based on the
-- timestamp.
newTerminologyProperties ::
  TerminologyProperties
newTerminologyProperties =
  TerminologyProperties'
    { encryptionKey =
        Core.Nothing,
      arn = Core.Nothing,
      targetLanguageCodes = Core.Nothing,
      createdAt = Core.Nothing,
      name = Core.Nothing,
      sizeBytes = Core.Nothing,
      description = Core.Nothing,
      termCount = Core.Nothing,
      sourceLanguageCode = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The encryption key for the custom terminology.
terminologyProperties_encryptionKey :: Lens.Lens' TerminologyProperties (Core.Maybe EncryptionKey)
terminologyProperties_encryptionKey = Lens.lens (\TerminologyProperties' {encryptionKey} -> encryptionKey) (\s@TerminologyProperties' {} a -> s {encryptionKey = a} :: TerminologyProperties)

-- | The Amazon Resource Name (ARN) of the custom terminology.
terminologyProperties_arn :: Lens.Lens' TerminologyProperties (Core.Maybe Core.Text)
terminologyProperties_arn = Lens.lens (\TerminologyProperties' {arn} -> arn) (\s@TerminologyProperties' {} a -> s {arn = a} :: TerminologyProperties)

-- | The language codes for the target languages available with the custom
-- terminology file. All possible target languages are returned in array.
terminologyProperties_targetLanguageCodes :: Lens.Lens' TerminologyProperties (Core.Maybe [Core.Text])
terminologyProperties_targetLanguageCodes = Lens.lens (\TerminologyProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@TerminologyProperties' {} a -> s {targetLanguageCodes = a} :: TerminologyProperties) Core.. Lens.mapping Lens._Coerce

-- | The time at which the custom terminology was created, based on the
-- timestamp.
terminologyProperties_createdAt :: Lens.Lens' TerminologyProperties (Core.Maybe Core.UTCTime)
terminologyProperties_createdAt = Lens.lens (\TerminologyProperties' {createdAt} -> createdAt) (\s@TerminologyProperties' {} a -> s {createdAt = a} :: TerminologyProperties) Core.. Lens.mapping Core._Time

-- | The name of the custom terminology.
terminologyProperties_name :: Lens.Lens' TerminologyProperties (Core.Maybe Core.Text)
terminologyProperties_name = Lens.lens (\TerminologyProperties' {name} -> name) (\s@TerminologyProperties' {} a -> s {name = a} :: TerminologyProperties)

-- | The size of the file used when importing a custom terminology.
terminologyProperties_sizeBytes :: Lens.Lens' TerminologyProperties (Core.Maybe Core.Int)
terminologyProperties_sizeBytes = Lens.lens (\TerminologyProperties' {sizeBytes} -> sizeBytes) (\s@TerminologyProperties' {} a -> s {sizeBytes = a} :: TerminologyProperties)

-- | The description of the custom terminology properties.
terminologyProperties_description :: Lens.Lens' TerminologyProperties (Core.Maybe Core.Text)
terminologyProperties_description = Lens.lens (\TerminologyProperties' {description} -> description) (\s@TerminologyProperties' {} a -> s {description = a} :: TerminologyProperties)

-- | The number of terms included in the custom terminology.
terminologyProperties_termCount :: Lens.Lens' TerminologyProperties (Core.Maybe Core.Int)
terminologyProperties_termCount = Lens.lens (\TerminologyProperties' {termCount} -> termCount) (\s@TerminologyProperties' {} a -> s {termCount = a} :: TerminologyProperties)

-- | The language code for the source text of the translation request for
-- which the custom terminology is being used.
terminologyProperties_sourceLanguageCode :: Lens.Lens' TerminologyProperties (Core.Maybe Core.Text)
terminologyProperties_sourceLanguageCode = Lens.lens (\TerminologyProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@TerminologyProperties' {} a -> s {sourceLanguageCode = a} :: TerminologyProperties)

-- | The time at which the custom terminology was last update, based on the
-- timestamp.
terminologyProperties_lastUpdatedAt :: Lens.Lens' TerminologyProperties (Core.Maybe Core.UTCTime)
terminologyProperties_lastUpdatedAt = Lens.lens (\TerminologyProperties' {lastUpdatedAt} -> lastUpdatedAt) (\s@TerminologyProperties' {} a -> s {lastUpdatedAt = a} :: TerminologyProperties) Core.. Lens.mapping Core._Time

instance Core.FromJSON TerminologyProperties where
  parseJSON =
    Core.withObject
      "TerminologyProperties"
      ( \x ->
          TerminologyProperties'
            Core.<$> (x Core..:? "EncryptionKey")
            Core.<*> (x Core..:? "Arn")
            Core.<*> ( x Core..:? "TargetLanguageCodes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "SizeBytes")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "TermCount")
            Core.<*> (x Core..:? "SourceLanguageCode")
            Core.<*> (x Core..:? "LastUpdatedAt")
      )

instance Core.Hashable TerminologyProperties

instance Core.NFData TerminologyProperties
