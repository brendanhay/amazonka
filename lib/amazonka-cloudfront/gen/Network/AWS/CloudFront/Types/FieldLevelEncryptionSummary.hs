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
    flesComment,
    flesId,
    flesLastModifiedTime,
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
  { queryArgProfileConfig ::
      Lude.Maybe QueryArgProfileConfig,
    contentTypeProfileConfig ::
      Lude.Maybe ContentTypeProfileConfig,
    comment :: Lude.Maybe Lude.Text,
    id :: Lude.Text,
    lastModifiedTime :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionSummary' with the minimum fields required to make a request.
--
-- * 'comment' - An optional comment about the field-level encryption item.
-- * 'contentTypeProfileConfig' - A summary of a content type-profile mapping.
-- * 'id' - The unique ID of a field-level encryption item.
-- * 'lastModifiedTime' - The last time that the summary of field-level encryption items was modified.
-- * 'queryArgProfileConfig' - A summary of a query argument-profile mapping.
mkFieldLevelEncryptionSummary ::
  -- | 'id'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.ISO8601 ->
  FieldLevelEncryptionSummary
mkFieldLevelEncryptionSummary pId_ pLastModifiedTime_ =
  FieldLevelEncryptionSummary'
    { queryArgProfileConfig =
        Lude.Nothing,
      contentTypeProfileConfig = Lude.Nothing,
      comment = Lude.Nothing,
      id = pId_,
      lastModifiedTime = pLastModifiedTime_
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

-- | An optional comment about the field-level encryption item.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesComment :: Lens.Lens' FieldLevelEncryptionSummary (Lude.Maybe Lude.Text)
flesComment = Lens.lens (comment :: FieldLevelEncryptionSummary -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The unique ID of a field-level encryption item.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesId :: Lens.Lens' FieldLevelEncryptionSummary Lude.Text
flesId = Lens.lens (id :: FieldLevelEncryptionSummary -> Lude.Text) (\s a -> s {id = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The last time that the summary of field-level encryption items was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flesLastModifiedTime :: Lens.Lens' FieldLevelEncryptionSummary Lude.ISO8601
flesLastModifiedTime = Lens.lens (lastModifiedTime :: FieldLevelEncryptionSummary -> Lude.ISO8601) (\s a -> s {lastModifiedTime = a} :: FieldLevelEncryptionSummary)
{-# DEPRECATED flesLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Lude.FromXML FieldLevelEncryptionSummary where
  parseXML x =
    FieldLevelEncryptionSummary'
      Lude.<$> (x Lude..@? "QueryArgProfileConfig")
      Lude.<*> (x Lude..@? "ContentTypeProfileConfig")
      Lude.<*> (x Lude..@? "Comment")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "LastModifiedTime")
