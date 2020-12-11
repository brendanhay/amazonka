-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.RedactionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.RedactionConfig
  ( RedactionConfig (..),

    -- * Smart constructor
    mkRedactionConfig,

    -- * Lenses
    rcMaskCharacter,
    rcMaskMode,
    rcPiiEntityTypes,
  )
where

import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Network.AWS.Comprehend.Types.PiiEntityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration parameters for PII entity redaction.
--
-- /See:/ 'mkRedactionConfig' smart constructor.
data RedactionConfig = RedactionConfig'
  { maskCharacter ::
      Lude.Maybe Lude.Text,
    maskMode :: Lude.Maybe PiiEntitiesDetectionMaskMode,
    piiEntityTypes :: Lude.Maybe [PiiEntityType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedactionConfig' with the minimum fields required to make a request.
--
-- * 'maskCharacter' - A character that replaces each character in the redacted PII entity.
-- * 'maskMode' - Specifies whether the PII entity is redacted with the mask character or the entity type.
-- * 'piiEntityTypes' - An array of the types of PII entities that Amazon Comprehend detects in the input text for your request.
mkRedactionConfig ::
  RedactionConfig
mkRedactionConfig =
  RedactionConfig'
    { maskCharacter = Lude.Nothing,
      maskMode = Lude.Nothing,
      piiEntityTypes = Lude.Nothing
    }

-- | A character that replaces each character in the redacted PII entity.
--
-- /Note:/ Consider using 'maskCharacter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaskCharacter :: Lens.Lens' RedactionConfig (Lude.Maybe Lude.Text)
rcMaskCharacter = Lens.lens (maskCharacter :: RedactionConfig -> Lude.Maybe Lude.Text) (\s a -> s {maskCharacter = a} :: RedactionConfig)
{-# DEPRECATED rcMaskCharacter "Use generic-lens or generic-optics with 'maskCharacter' instead." #-}

-- | Specifies whether the PII entity is redacted with the mask character or the entity type.
--
-- /Note:/ Consider using 'maskMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaskMode :: Lens.Lens' RedactionConfig (Lude.Maybe PiiEntitiesDetectionMaskMode)
rcMaskMode = Lens.lens (maskMode :: RedactionConfig -> Lude.Maybe PiiEntitiesDetectionMaskMode) (\s a -> s {maskMode = a} :: RedactionConfig)
{-# DEPRECATED rcMaskMode "Use generic-lens or generic-optics with 'maskMode' instead." #-}

-- | An array of the types of PII entities that Amazon Comprehend detects in the input text for your request.
--
-- /Note:/ Consider using 'piiEntityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPiiEntityTypes :: Lens.Lens' RedactionConfig (Lude.Maybe [PiiEntityType])
rcPiiEntityTypes = Lens.lens (piiEntityTypes :: RedactionConfig -> Lude.Maybe [PiiEntityType]) (\s a -> s {piiEntityTypes = a} :: RedactionConfig)
{-# DEPRECATED rcPiiEntityTypes "Use generic-lens or generic-optics with 'piiEntityTypes' instead." #-}

instance Lude.FromJSON RedactionConfig where
  parseJSON =
    Lude.withObject
      "RedactionConfig"
      ( \x ->
          RedactionConfig'
            Lude.<$> (x Lude..:? "MaskCharacter")
            Lude.<*> (x Lude..:? "MaskMode")
            Lude.<*> (x Lude..:? "PiiEntityTypes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON RedactionConfig where
  toJSON RedactionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaskCharacter" Lude..=) Lude.<$> maskCharacter,
            ("MaskMode" Lude..=) Lude.<$> maskMode,
            ("PiiEntityTypes" Lude..=) Lude.<$> piiEntityTypes
          ]
      )
