-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
  ( FieldLevelEncryptionProfileConfig (..),

    -- * Smart constructor
    mkFieldLevelEncryptionProfileConfig,

    -- * Lenses
    flepcComment,
    flepcName,
    flepcCallerReference,
    flepcEncryptionEntities,
  )
where

import Network.AWS.CloudFront.Types.EncryptionEntities
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type of profiles for the field-level encryption.
--
-- /See:/ 'mkFieldLevelEncryptionProfileConfig' smart constructor.
data FieldLevelEncryptionProfileConfig = FieldLevelEncryptionProfileConfig'
  { comment ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    callerReference ::
      Lude.Text,
    encryptionEntities ::
      EncryptionEntities
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionProfileConfig' with the minimum fields required to make a request.
--
-- * 'callerReference' - A unique number that ensures that the request can't be replayed.
-- * 'comment' - An optional comment for the field-level encryption profile.
-- * 'encryptionEntities' - A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
-- * 'name' - Profile name for the field-level encryption profile.
mkFieldLevelEncryptionProfileConfig ::
  -- | 'name'
  Lude.Text ->
  -- | 'callerReference'
  Lude.Text ->
  -- | 'encryptionEntities'
  EncryptionEntities ->
  FieldLevelEncryptionProfileConfig
mkFieldLevelEncryptionProfileConfig
  pName_
  pCallerReference_
  pEncryptionEntities_ =
    FieldLevelEncryptionProfileConfig'
      { comment = Lude.Nothing,
        name = pName_,
        callerReference = pCallerReference_,
        encryptionEntities = pEncryptionEntities_
      }

-- | An optional comment for the field-level encryption profile.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcComment :: Lens.Lens' FieldLevelEncryptionProfileConfig (Lude.Maybe Lude.Text)
flepcComment = Lens.lens (comment :: FieldLevelEncryptionProfileConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: FieldLevelEncryptionProfileConfig)
{-# DEPRECATED flepcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | Profile name for the field-level encryption profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcName :: Lens.Lens' FieldLevelEncryptionProfileConfig Lude.Text
flepcName = Lens.lens (name :: FieldLevelEncryptionProfileConfig -> Lude.Text) (\s a -> s {name = a} :: FieldLevelEncryptionProfileConfig)
{-# DEPRECATED flepcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique number that ensures that the request can't be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcCallerReference :: Lens.Lens' FieldLevelEncryptionProfileConfig Lude.Text
flepcCallerReference = Lens.lens (callerReference :: FieldLevelEncryptionProfileConfig -> Lude.Text) (\s a -> s {callerReference = a} :: FieldLevelEncryptionProfileConfig)
{-# DEPRECATED flepcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
--
-- /Note:/ Consider using 'encryptionEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcEncryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileConfig EncryptionEntities
flepcEncryptionEntities = Lens.lens (encryptionEntities :: FieldLevelEncryptionProfileConfig -> EncryptionEntities) (\s a -> s {encryptionEntities = a} :: FieldLevelEncryptionProfileConfig)
{-# DEPRECATED flepcEncryptionEntities "Use generic-lens or generic-optics with 'encryptionEntities' instead." #-}

instance Lude.FromXML FieldLevelEncryptionProfileConfig where
  parseXML x =
    FieldLevelEncryptionProfileConfig'
      Lude.<$> (x Lude..@? "Comment")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "CallerReference")
      Lude.<*> (x Lude..@ "EncryptionEntities")

instance Lude.ToXML FieldLevelEncryptionProfileConfig where
  toXML FieldLevelEncryptionProfileConfig' {..} =
    Lude.mconcat
      [ "Comment" Lude.@= comment,
        "Name" Lude.@= name,
        "CallerReference" Lude.@= callerReference,
        "EncryptionEntities" Lude.@= encryptionEntities
      ]
