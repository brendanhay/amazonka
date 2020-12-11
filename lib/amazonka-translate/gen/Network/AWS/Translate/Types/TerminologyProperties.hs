-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyProperties
  ( TerminologyProperties (..),

    -- * Smart constructor
    mkTerminologyProperties,

    -- * Lenses
    tpSizeBytes,
    tpLastUpdatedAt,
    tpARN,
    tpTargetLanguageCodes,
    tpCreatedAt,
    tpName,
    tpSourceLanguageCode,
    tpTermCount,
    tpEncryptionKey,
    tpDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.EncryptionKey

-- | The properties of the custom terminology.
--
-- /See:/ 'mkTerminologyProperties' smart constructor.
data TerminologyProperties = TerminologyProperties'
  { sizeBytes ::
      Lude.Maybe Lude.Int,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    targetLanguageCodes :: Lude.Maybe [Lude.Text],
    createdAt :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    sourceLanguageCode :: Lude.Maybe Lude.Text,
    termCount :: Lude.Maybe Lude.Int,
    encryptionKey :: Lude.Maybe EncryptionKey,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminologyProperties' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the custom terminology.
-- * 'createdAt' - The time at which the custom terminology was created, based on the timestamp.
-- * 'description' - The description of the custom terminology properties.
-- * 'encryptionKey' - The encryption key for the custom terminology.
-- * 'lastUpdatedAt' - The time at which the custom terminology was last update, based on the timestamp.
-- * 'name' - The name of the custom terminology.
-- * 'sizeBytes' - The size of the file used when importing a custom terminology.
-- * 'sourceLanguageCode' - The language code for the source text of the translation request for which the custom terminology is being used.
-- * 'targetLanguageCodes' - The language codes for the target languages available with the custom terminology file. All possible target languages are returned in array.
-- * 'termCount' - The number of terms included in the custom terminology.
mkTerminologyProperties ::
  TerminologyProperties
mkTerminologyProperties =
  TerminologyProperties'
    { sizeBytes = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      arn = Lude.Nothing,
      targetLanguageCodes = Lude.Nothing,
      createdAt = Lude.Nothing,
      name = Lude.Nothing,
      sourceLanguageCode = Lude.Nothing,
      termCount = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The size of the file used when importing a custom terminology.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpSizeBytes :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Int)
tpSizeBytes = Lens.lens (sizeBytes :: TerminologyProperties -> Lude.Maybe Lude.Int) (\s a -> s {sizeBytes = a} :: TerminologyProperties)
{-# DEPRECATED tpSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The time at which the custom terminology was last update, based on the timestamp.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpLastUpdatedAt :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Timestamp)
tpLastUpdatedAt = Lens.lens (lastUpdatedAt :: TerminologyProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: TerminologyProperties)
{-# DEPRECATED tpLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the custom terminology.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpARN :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Text)
tpARN = Lens.lens (arn :: TerminologyProperties -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TerminologyProperties)
{-# DEPRECATED tpARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The language codes for the target languages available with the custom terminology file. All possible target languages are returned in array.
--
-- /Note:/ Consider using 'targetLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpTargetLanguageCodes :: Lens.Lens' TerminologyProperties (Lude.Maybe [Lude.Text])
tpTargetLanguageCodes = Lens.lens (targetLanguageCodes :: TerminologyProperties -> Lude.Maybe [Lude.Text]) (\s a -> s {targetLanguageCodes = a} :: TerminologyProperties)
{-# DEPRECATED tpTargetLanguageCodes "Use generic-lens or generic-optics with 'targetLanguageCodes' instead." #-}

-- | The time at which the custom terminology was created, based on the timestamp.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpCreatedAt :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Timestamp)
tpCreatedAt = Lens.lens (createdAt :: TerminologyProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: TerminologyProperties)
{-# DEPRECATED tpCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The name of the custom terminology.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpName :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Text)
tpName = Lens.lens (name :: TerminologyProperties -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TerminologyProperties)
{-# DEPRECATED tpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The language code for the source text of the translation request for which the custom terminology is being used.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpSourceLanguageCode :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Text)
tpSourceLanguageCode = Lens.lens (sourceLanguageCode :: TerminologyProperties -> Lude.Maybe Lude.Text) (\s a -> s {sourceLanguageCode = a} :: TerminologyProperties)
{-# DEPRECATED tpSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The number of terms included in the custom terminology.
--
-- /Note:/ Consider using 'termCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpTermCount :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Int)
tpTermCount = Lens.lens (termCount :: TerminologyProperties -> Lude.Maybe Lude.Int) (\s a -> s {termCount = a} :: TerminologyProperties)
{-# DEPRECATED tpTermCount "Use generic-lens or generic-optics with 'termCount' instead." #-}

-- | The encryption key for the custom terminology.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpEncryptionKey :: Lens.Lens' TerminologyProperties (Lude.Maybe EncryptionKey)
tpEncryptionKey = Lens.lens (encryptionKey :: TerminologyProperties -> Lude.Maybe EncryptionKey) (\s a -> s {encryptionKey = a} :: TerminologyProperties)
{-# DEPRECATED tpEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | The description of the custom terminology properties.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDescription :: Lens.Lens' TerminologyProperties (Lude.Maybe Lude.Text)
tpDescription = Lens.lens (description :: TerminologyProperties -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TerminologyProperties)
{-# DEPRECATED tpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON TerminologyProperties where
  parseJSON =
    Lude.withObject
      "TerminologyProperties"
      ( \x ->
          TerminologyProperties'
            Lude.<$> (x Lude..:? "SizeBytes")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "TargetLanguageCodes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "SourceLanguageCode")
            Lude.<*> (x Lude..:? "TermCount")
            Lude.<*> (x Lude..:? "EncryptionKey")
            Lude.<*> (x Lude..:? "Description")
      )
