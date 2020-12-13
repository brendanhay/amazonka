{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
  ( FieldLevelEncryptionConfig (..),

    -- * Smart constructor
    mkFieldLevelEncryptionConfig,

    -- * Lenses
    flecQueryArgProfileConfig,
    flecContentTypeProfileConfig,
    flecComment,
    flecCallerReference,
  )
where

import Network.AWS.CloudFront.Types.ContentTypeProfileConfig
import Network.AWS.CloudFront.Types.QueryArgProfileConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type that includes the profile configurations specified for field-level encryption.
--
-- /See:/ 'mkFieldLevelEncryptionConfig' smart constructor.
data FieldLevelEncryptionConfig = FieldLevelEncryptionConfig'
  { -- | A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
    queryArgProfileConfig :: Lude.Maybe QueryArgProfileConfig,
    -- | A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
    contentTypeProfileConfig :: Lude.Maybe ContentTypeProfileConfig,
    -- | An optional comment about the configuration.
    comment :: Lude.Maybe Lude.Text,
    -- | A unique number that ensures the request can't be replayed.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'queryArgProfileConfig' - A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
-- * 'contentTypeProfileConfig' - A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
-- * 'comment' - An optional comment about the configuration.
-- * 'callerReference' - A unique number that ensures the request can't be replayed.
mkFieldLevelEncryptionConfig ::
  -- | 'callerReference'
  Lude.Text ->
  FieldLevelEncryptionConfig
mkFieldLevelEncryptionConfig pCallerReference_ =
  FieldLevelEncryptionConfig'
    { queryArgProfileConfig = Lude.Nothing,
      contentTypeProfileConfig = Lude.Nothing,
      comment = Lude.Nothing,
      callerReference = pCallerReference_
    }

-- | A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
--
-- /Note:/ Consider using 'queryArgProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecQueryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Lude.Maybe QueryArgProfileConfig)
flecQueryArgProfileConfig = Lens.lens (queryArgProfileConfig :: FieldLevelEncryptionConfig -> Lude.Maybe QueryArgProfileConfig) (\s a -> s {queryArgProfileConfig = a} :: FieldLevelEncryptionConfig)
{-# DEPRECATED flecQueryArgProfileConfig "Use generic-lens or generic-optics with 'queryArgProfileConfig' instead." #-}

-- | A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
--
-- /Note:/ Consider using 'contentTypeProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecContentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Lude.Maybe ContentTypeProfileConfig)
flecContentTypeProfileConfig = Lens.lens (contentTypeProfileConfig :: FieldLevelEncryptionConfig -> Lude.Maybe ContentTypeProfileConfig) (\s a -> s {contentTypeProfileConfig = a} :: FieldLevelEncryptionConfig)
{-# DEPRECATED flecContentTypeProfileConfig "Use generic-lens or generic-optics with 'contentTypeProfileConfig' instead." #-}

-- | An optional comment about the configuration.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecComment :: Lens.Lens' FieldLevelEncryptionConfig (Lude.Maybe Lude.Text)
flecComment = Lens.lens (comment :: FieldLevelEncryptionConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: FieldLevelEncryptionConfig)
{-# DEPRECATED flecComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A unique number that ensures the request can't be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flecCallerReference :: Lens.Lens' FieldLevelEncryptionConfig Lude.Text
flecCallerReference = Lens.lens (callerReference :: FieldLevelEncryptionConfig -> Lude.Text) (\s a -> s {callerReference = a} :: FieldLevelEncryptionConfig)
{-# DEPRECATED flecCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.FromXML FieldLevelEncryptionConfig where
  parseXML x =
    FieldLevelEncryptionConfig'
      Lude.<$> (x Lude..@? "QueryArgProfileConfig")
      Lude.<*> (x Lude..@? "ContentTypeProfileConfig")
      Lude.<*> (x Lude..@? "Comment")
      Lude.<*> (x Lude..@ "CallerReference")

instance Lude.ToXML FieldLevelEncryptionConfig where
  toXML FieldLevelEncryptionConfig' {..} =
    Lude.mconcat
      [ "QueryArgProfileConfig" Lude.@= queryArgProfileConfig,
        "ContentTypeProfileConfig" Lude.@= contentTypeProfileConfig,
        "Comment" Lude.@= comment,
        "CallerReference" Lude.@= callerReference
      ]
