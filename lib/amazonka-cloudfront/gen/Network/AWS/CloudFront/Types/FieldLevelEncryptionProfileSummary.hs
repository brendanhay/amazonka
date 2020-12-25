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
    flepsId,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,
    flepsComment,
  )
where

import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.EncryptionEntities as Types
import qualified Network.AWS.CloudFront.Types.Id as Types
import qualified Network.AWS.CloudFront.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The field-level encryption profile summary.
--
-- /See:/ 'mkFieldLevelEncryptionProfileSummary' smart constructor.
data FieldLevelEncryptionProfileSummary = FieldLevelEncryptionProfileSummary'
  { -- | ID for the field-level encryption profile summary.
    id :: Types.Id,
    -- | The time when the the field-level encryption profile summary was last updated.
    lastModifiedTime :: Core.UTCTime,
    -- | Name for the field-level encryption profile summary.
    name :: Types.Name,
    -- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
    encryptionEntities :: Types.EncryptionEntities,
    -- | An optional comment for the field-level encryption profile summary.
    comment :: Core.Maybe Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FieldLevelEncryptionProfileSummary' value with any optional fields omitted.
mkFieldLevelEncryptionProfileSummary ::
  -- | 'id'
  Types.Id ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'name'
  Types.Name ->
  -- | 'encryptionEntities'
  Types.EncryptionEntities ->
  FieldLevelEncryptionProfileSummary
mkFieldLevelEncryptionProfileSummary
  id
  lastModifiedTime
  name
  encryptionEntities =
    FieldLevelEncryptionProfileSummary'
      { id,
        lastModifiedTime,
        name,
        encryptionEntities,
        comment = Core.Nothing
      }

-- | ID for the field-level encryption profile summary.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsId :: Lens.Lens' FieldLevelEncryptionProfileSummary Types.Id
flepsId = Lens.field @"id"
{-# DEPRECATED flepsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time when the the field-level encryption profile summary was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsLastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfileSummary Core.UTCTime
flepsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED flepsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Name for the field-level encryption profile summary.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsName :: Lens.Lens' FieldLevelEncryptionProfileSummary Types.Name
flepsName = Lens.field @"name"
{-# DEPRECATED flepsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
--
-- /Note:/ Consider using 'encryptionEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsEncryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileSummary Types.EncryptionEntities
flepsEncryptionEntities = Lens.field @"encryptionEntities"
{-# DEPRECATED flepsEncryptionEntities "Use generic-lens or generic-optics with 'encryptionEntities' instead." #-}

-- | An optional comment for the field-level encryption profile summary.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepsComment :: Lens.Lens' FieldLevelEncryptionProfileSummary (Core.Maybe Types.Comment)
flepsComment = Lens.field @"comment"
{-# DEPRECATED flepsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.FromXML FieldLevelEncryptionProfileSummary where
  parseXML x =
    FieldLevelEncryptionProfileSummary'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "EncryptionEntities")
      Core.<*> (x Core..@? "Comment")
