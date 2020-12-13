{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
  ( FieldLevelEncryptionProfileSummary (..),

    -- * Smart constructor
    mkFieldLevelEncryptionProfileSummary,

    -- * Lenses
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,
    flepsId,
    flepsComment,
  )
where

import Network.AWS.CloudFront.Types.EncryptionEntities
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The field-level encryption profile summary.
--
-- /See:/ 'mkFieldLevelEncryptionProfileSummary' smart constructor.
data FieldLevelEncryptionProfileSummary = FieldLevelEncryptionProfileSummary'
  { -- | The time when the the field-level encryption profile summary was last updated.
    lastModifiedTime :: Lude.DateTime,
    -- | Name for the field-level encryption profile summary.
    name :: Lude.Text,
    -- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
    encryptionEntities :: EncryptionEntities,
    -- | ID for the field-level encryption profile summary.
    id :: Lude.Text,
    -- | An optional comment for the field-level encryption profile summary.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionProfileSummary' with the minimum fields required to make a request.
--
-- * 'lastModifiedTime' - The time when the the field-level encryption profile summary was last updated.
-- * 'name' - Name for the field-level encryption profile summary.
-- * 'encryptionEntities' - A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
-- * 'id' - ID for the field-level encryption profile summary.
-- * 'comment' - An optional comment for the field-level encryption profile summary.
mkFieldLevelEncryptionProfileSummary ::
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'name'
  Lude.Text ->
  -- | 'encryptionEntities'
  EncryptionEntities ->
  -- | 'id'
  Lude.Text ->
  FieldLevelEncryptionProfileSummary
mkFieldLevelEncryptionProfileSummary
  pLastModifiedTime_
  pName_
  pEncryptionEntities_
  pId_ =
    FieldLevelEncryptionProfileSummary'
      { lastModifiedTime =
          pLastModifiedTime_,
        name = pName_,
        encryptionEntities = pEncryptionEntities_,
        id = pId_,
        comment = Lude.Nothing
      }

-- | The time when the the field-level encryption profile summary was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsLastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfileSummary Lude.DateTime
flepsLastModifiedTime = Lens.lens (lastModifiedTime :: FieldLevelEncryptionProfileSummary -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: FieldLevelEncryptionProfileSummary)
{-# DEPRECATED flepsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Name for the field-level encryption profile summary.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsName :: Lens.Lens' FieldLevelEncryptionProfileSummary Lude.Text
flepsName = Lens.lens (name :: FieldLevelEncryptionProfileSummary -> Lude.Text) (\s a -> s {name = a} :: FieldLevelEncryptionProfileSummary)
{-# DEPRECATED flepsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
--
-- /Note:/ Consider using 'encryptionEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsEncryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileSummary EncryptionEntities
flepsEncryptionEntities = Lens.lens (encryptionEntities :: FieldLevelEncryptionProfileSummary -> EncryptionEntities) (\s a -> s {encryptionEntities = a} :: FieldLevelEncryptionProfileSummary)
{-# DEPRECATED flepsEncryptionEntities "Use generic-lens or generic-optics with 'encryptionEntities' instead." #-}

-- | ID for the field-level encryption profile summary.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsId :: Lens.Lens' FieldLevelEncryptionProfileSummary Lude.Text
flepsId = Lens.lens (id :: FieldLevelEncryptionProfileSummary -> Lude.Text) (\s a -> s {id = a} :: FieldLevelEncryptionProfileSummary)
{-# DEPRECATED flepsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An optional comment for the field-level encryption profile summary.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsComment :: Lens.Lens' FieldLevelEncryptionProfileSummary (Lude.Maybe Lude.Text)
flepsComment = Lens.lens (comment :: FieldLevelEncryptionProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: FieldLevelEncryptionProfileSummary)
{-# DEPRECATED flepsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML FieldLevelEncryptionProfileSummary where
  parseXML x =
    FieldLevelEncryptionProfileSummary'
      Lude.<$> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "EncryptionEntities")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "Comment")
