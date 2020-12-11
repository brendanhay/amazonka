-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EncryptionAtRest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionAtRest
  ( EncryptionAtRest (..),

    -- * Smart constructor
    mkEncryptionAtRest,

    -- * Lenses
    earSseAWSKMSKeyId,
    earCatalogEncryptionMode,
  )
where

import Network.AWS.Glue.Types.CatalogEncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- /See:/ 'mkEncryptionAtRest' smart constructor.
data EncryptionAtRest = EncryptionAtRest'
  { sseAWSKMSKeyId ::
      Lude.Maybe Lude.Text,
    catalogEncryptionMode :: CatalogEncryptionMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionAtRest' with the minimum fields required to make a request.
--
-- * 'catalogEncryptionMode' - The encryption-at-rest mode for encrypting Data Catalog data.
-- * 'sseAWSKMSKeyId' - The ID of the AWS KMS key to use for encryption at rest.
mkEncryptionAtRest ::
  -- | 'catalogEncryptionMode'
  CatalogEncryptionMode ->
  EncryptionAtRest
mkEncryptionAtRest pCatalogEncryptionMode_ =
  EncryptionAtRest'
    { sseAWSKMSKeyId = Lude.Nothing,
      catalogEncryptionMode = pCatalogEncryptionMode_
    }

-- | The ID of the AWS KMS key to use for encryption at rest.
--
-- /Note:/ Consider using 'sseAWSKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earSseAWSKMSKeyId :: Lens.Lens' EncryptionAtRest (Lude.Maybe Lude.Text)
earSseAWSKMSKeyId = Lens.lens (sseAWSKMSKeyId :: EncryptionAtRest -> Lude.Maybe Lude.Text) (\s a -> s {sseAWSKMSKeyId = a} :: EncryptionAtRest)
{-# DEPRECATED earSseAWSKMSKeyId "Use generic-lens or generic-optics with 'sseAWSKMSKeyId' instead." #-}

-- | The encryption-at-rest mode for encrypting Data Catalog data.
--
-- /Note:/ Consider using 'catalogEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earCatalogEncryptionMode :: Lens.Lens' EncryptionAtRest CatalogEncryptionMode
earCatalogEncryptionMode = Lens.lens (catalogEncryptionMode :: EncryptionAtRest -> CatalogEncryptionMode) (\s a -> s {catalogEncryptionMode = a} :: EncryptionAtRest)
{-# DEPRECATED earCatalogEncryptionMode "Use generic-lens or generic-optics with 'catalogEncryptionMode' instead." #-}

instance Lude.FromJSON EncryptionAtRest where
  parseJSON =
    Lude.withObject
      "EncryptionAtRest"
      ( \x ->
          EncryptionAtRest'
            Lude.<$> (x Lude..:? "SseAwsKmsKeyId")
            Lude.<*> (x Lude..: "CatalogEncryptionMode")
      )

instance Lude.ToJSON EncryptionAtRest where
  toJSON EncryptionAtRest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SseAwsKmsKeyId" Lude..=) Lude.<$> sseAWSKMSKeyId,
            Lude.Just ("CatalogEncryptionMode" Lude..= catalogEncryptionMode)
          ]
      )
