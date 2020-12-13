{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryption
  ( FieldLevelEncryption (..),

    -- * Smart constructor
    mkFieldLevelEncryption,

    -- * Lenses
    fleLastModifiedTime,
    fleId,
    fleFieldLevelEncryptionConfig,
  )
where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type that includes the profile configurations and other options specified for field-level encryption.
--
-- /See:/ 'mkFieldLevelEncryption' smart constructor.
data FieldLevelEncryption = FieldLevelEncryption'
  { -- | The last time the field-level encryption configuration was changed.
    lastModifiedTime :: Lude.DateTime,
    -- | The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
    id :: Lude.Text,
    -- | A complex data type that includes the profile configurations specified for field-level encryption.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryption' with the minimum fields required to make a request.
--
-- * 'lastModifiedTime' - The last time the field-level encryption configuration was changed.
-- * 'id' - The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
-- * 'fieldLevelEncryptionConfig' - A complex data type that includes the profile configurations specified for field-level encryption.
mkFieldLevelEncryption ::
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'id'
  Lude.Text ->
  -- | 'fieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  FieldLevelEncryption
mkFieldLevelEncryption
  pLastModifiedTime_
  pId_
  pFieldLevelEncryptionConfig_ =
    FieldLevelEncryption'
      { lastModifiedTime = pLastModifiedTime_,
        id = pId_,
        fieldLevelEncryptionConfig = pFieldLevelEncryptionConfig_
      }

-- | The last time the field-level encryption configuration was changed.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLastModifiedTime :: Lens.Lens' FieldLevelEncryption Lude.DateTime
fleLastModifiedTime = Lens.lens (lastModifiedTime :: FieldLevelEncryption -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: FieldLevelEncryption)
{-# DEPRECATED fleLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleId :: Lens.Lens' FieldLevelEncryption Lude.Text
fleId = Lens.lens (id :: FieldLevelEncryption -> Lude.Text) (\s a -> s {id = a} :: FieldLevelEncryption)
{-# DEPRECATED fleId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A complex data type that includes the profile configurations specified for field-level encryption.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleFieldLevelEncryptionConfig :: Lens.Lens' FieldLevelEncryption FieldLevelEncryptionConfig
fleFieldLevelEncryptionConfig = Lens.lens (fieldLevelEncryptionConfig :: FieldLevelEncryption -> FieldLevelEncryptionConfig) (\s a -> s {fieldLevelEncryptionConfig = a} :: FieldLevelEncryption)
{-# DEPRECATED fleFieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead." #-}

instance Lude.FromXML FieldLevelEncryption where
  parseXML x =
    FieldLevelEncryption'
      Lude.<$> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "FieldLevelEncryptionConfig")
