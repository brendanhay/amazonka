{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
  ( FieldLevelEncryptionSummary (..),

    -- * Smart constructor
    mkFieldLevelEncryptionSummary,

    -- * Lenses
    flesQueryArgProfileConfig,
    flesContentTypeProfileConfig,
    flesLastModifiedTime,
    flesId,
    flesComment,
  )
where

import Network.AWS.CloudFront.Types.ContentTypeProfileConfig
import Network.AWS.CloudFront.Types.QueryArgProfileConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of a field-level encryption item.
--
-- /See:/ 'mkFieldLevelEncryptionSummary' smart constructor.
data FieldLevelEncryptionSummary = FieldLevelEncryptionSummary'
  { -- | A summary of a query argument-profile mapping.
    queryArgProfileConfig :: Lude.Maybe QueryArgProfileConfig,
    -- | A summary of a content type-profile mapping.
    contentTypeProfileConfig :: Lude.Maybe ContentTypeProfileConfig,
    -- | The last time that the summary of field-level encryption items was modified.
    lastModifiedTime :: Lude.DateTime,
    -- | The unique ID of a field-level encryption item.
    id :: Lude.Text,
    -- | An optional comment about the field-level encryption item.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionSummary' with the minimum fields required to make a request.
--
-- * 'queryArgProfileConfig' - A summary of a query argument-profile mapping.
-- * 'contentTypeProfileConfig' - A summary of a content type-profile mapping.
-- * 'lastModifiedTime' - The last time that the summary of field-level encryption items was modified.
-- * 'id' - The unique ID of a field-level encryption item.
-- * 'comment' - An optional comment about the field-level encryption item.
mkFieldLevelEncryptionSummary ::
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'id'
  Lude.Text ->
  FieldLevelEncryptionSummary
mkFieldLevelEncryptionSummary pLastModifiedTime_ pId_ =
  FieldLevelEncryptionSummary'
    { queryArgProfileConfig =
        Lude.Nothing,
      contentTypeProfileConfig = Lude.Nothing,
      lastModifiedTime = pLastModifiedTime_,
      id = pId_,
      comment = Lude.Nothing
    }

-- | A summary of a query argument-profile mapping.
--
-- /Note:/ Consider using 'queryArgProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesQueryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Lude.Maybe QueryArgProfileConfig)
flesQueryArgProfileConfig = Lens.lens (queryArgProfileConfig :: FieldLevelEncryptionSummary -> Lude.Maybe QueryArgProfileConfig) (\s a -> s {queryArgProfileConfig = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesQueryArgProfileConfig "Use generic-lens or generic-optics with 'queryArgProfileConfig' instead." #-}

-- | A summary of a content type-profile mapping.
--
-- /Note:/ Consider using 'contentTypeProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesContentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Lude.Maybe ContentTypeProfileConfig)
flesContentTypeProfileConfig = Lens.lens (contentTypeProfileConfig :: FieldLevelEncryptionSummary -> Lude.Maybe ContentTypeProfileConfig) (\s a -> s {contentTypeProfileConfig = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesContentTypeProfileConfig "Use generic-lens or generic-optics with 'contentTypeProfileConfig' instead." #-}

-- | The last time that the summary of field-level encryption items was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesLastModifiedTime :: Lens.Lens' FieldLevelEncryptionSummary Lude.DateTime
flesLastModifiedTime = Lens.lens (lastModifiedTime :: FieldLevelEncryptionSummary -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The unique ID of a field-level encryption item.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesId :: Lens.Lens' FieldLevelEncryptionSummary Lude.Text
flesId = Lens.lens (id :: FieldLevelEncryptionSummary -> Lude.Text) (\s a -> s {id = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An optional comment about the field-level encryption item.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesComment :: Lens.Lens' FieldLevelEncryptionSummary (Lude.Maybe Lude.Text)
flesComment = Lens.lens (comment :: FieldLevelEncryptionSummary -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML FieldLevelEncryptionSummary where
  parseXML x =
    FieldLevelEncryptionSummary'
      Lude.<$> (x Lude..@? "QueryArgProfileConfig")
      Lude.<*> (x Lude..@? "ContentTypeProfileConfig")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "Comment")
