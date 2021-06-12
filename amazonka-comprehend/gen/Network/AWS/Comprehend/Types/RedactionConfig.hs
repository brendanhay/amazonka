{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.RedactionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.RedactionConfig where

import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Network.AWS.Comprehend.Types.PiiEntityType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides configuration parameters for PII entity redaction.
--
-- /See:/ 'newRedactionConfig' smart constructor.
data RedactionConfig = RedactionConfig'
  { -- | A character that replaces each character in the redacted PII entity.
    maskCharacter :: Core.Maybe Core.Text,
    -- | Specifies whether the PII entity is redacted with the mask character or
    -- the entity type.
    maskMode :: Core.Maybe PiiEntitiesDetectionMaskMode,
    -- | An array of the types of PII entities that Amazon Comprehend detects in
    -- the input text for your request.
    piiEntityTypes :: Core.Maybe [PiiEntityType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RedactionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maskCharacter', 'redactionConfig_maskCharacter' - A character that replaces each character in the redacted PII entity.
--
-- 'maskMode', 'redactionConfig_maskMode' - Specifies whether the PII entity is redacted with the mask character or
-- the entity type.
--
-- 'piiEntityTypes', 'redactionConfig_piiEntityTypes' - An array of the types of PII entities that Amazon Comprehend detects in
-- the input text for your request.
newRedactionConfig ::
  RedactionConfig
newRedactionConfig =
  RedactionConfig'
    { maskCharacter = Core.Nothing,
      maskMode = Core.Nothing,
      piiEntityTypes = Core.Nothing
    }

-- | A character that replaces each character in the redacted PII entity.
redactionConfig_maskCharacter :: Lens.Lens' RedactionConfig (Core.Maybe Core.Text)
redactionConfig_maskCharacter = Lens.lens (\RedactionConfig' {maskCharacter} -> maskCharacter) (\s@RedactionConfig' {} a -> s {maskCharacter = a} :: RedactionConfig)

-- | Specifies whether the PII entity is redacted with the mask character or
-- the entity type.
redactionConfig_maskMode :: Lens.Lens' RedactionConfig (Core.Maybe PiiEntitiesDetectionMaskMode)
redactionConfig_maskMode = Lens.lens (\RedactionConfig' {maskMode} -> maskMode) (\s@RedactionConfig' {} a -> s {maskMode = a} :: RedactionConfig)

-- | An array of the types of PII entities that Amazon Comprehend detects in
-- the input text for your request.
redactionConfig_piiEntityTypes :: Lens.Lens' RedactionConfig (Core.Maybe [PiiEntityType])
redactionConfig_piiEntityTypes = Lens.lens (\RedactionConfig' {piiEntityTypes} -> piiEntityTypes) (\s@RedactionConfig' {} a -> s {piiEntityTypes = a} :: RedactionConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON RedactionConfig where
  parseJSON =
    Core.withObject
      "RedactionConfig"
      ( \x ->
          RedactionConfig'
            Core.<$> (x Core..:? "MaskCharacter")
            Core.<*> (x Core..:? "MaskMode")
            Core.<*> (x Core..:? "PiiEntityTypes" Core..!= Core.mempty)
      )

instance Core.Hashable RedactionConfig

instance Core.NFData RedactionConfig

instance Core.ToJSON RedactionConfig where
  toJSON RedactionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaskCharacter" Core..=) Core.<$> maskCharacter,
            ("MaskMode" Core..=) Core.<$> maskMode,
            ("PiiEntityTypes" Core..=) Core.<$> piiEntityTypes
          ]
      )
