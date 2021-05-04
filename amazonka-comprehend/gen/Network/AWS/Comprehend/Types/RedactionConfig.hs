{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration parameters for PII entity redaction.
--
-- /See:/ 'newRedactionConfig' smart constructor.
data RedactionConfig = RedactionConfig'
  { -- | A character that replaces each character in the redacted PII entity.
    maskCharacter :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the PII entity is redacted with the mask character or
    -- the entity type.
    maskMode :: Prelude.Maybe PiiEntitiesDetectionMaskMode,
    -- | An array of the types of PII entities that Amazon Comprehend detects in
    -- the input text for your request.
    piiEntityTypes :: Prelude.Maybe [PiiEntityType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { maskCharacter = Prelude.Nothing,
      maskMode = Prelude.Nothing,
      piiEntityTypes = Prelude.Nothing
    }

-- | A character that replaces each character in the redacted PII entity.
redactionConfig_maskCharacter :: Lens.Lens' RedactionConfig (Prelude.Maybe Prelude.Text)
redactionConfig_maskCharacter = Lens.lens (\RedactionConfig' {maskCharacter} -> maskCharacter) (\s@RedactionConfig' {} a -> s {maskCharacter = a} :: RedactionConfig)

-- | Specifies whether the PII entity is redacted with the mask character or
-- the entity type.
redactionConfig_maskMode :: Lens.Lens' RedactionConfig (Prelude.Maybe PiiEntitiesDetectionMaskMode)
redactionConfig_maskMode = Lens.lens (\RedactionConfig' {maskMode} -> maskMode) (\s@RedactionConfig' {} a -> s {maskMode = a} :: RedactionConfig)

-- | An array of the types of PII entities that Amazon Comprehend detects in
-- the input text for your request.
redactionConfig_piiEntityTypes :: Lens.Lens' RedactionConfig (Prelude.Maybe [PiiEntityType])
redactionConfig_piiEntityTypes = Lens.lens (\RedactionConfig' {piiEntityTypes} -> piiEntityTypes) (\s@RedactionConfig' {} a -> s {piiEntityTypes = a} :: RedactionConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON RedactionConfig where
  parseJSON =
    Prelude.withObject
      "RedactionConfig"
      ( \x ->
          RedactionConfig'
            Prelude.<$> (x Prelude..:? "MaskCharacter")
            Prelude.<*> (x Prelude..:? "MaskMode")
            Prelude.<*> ( x Prelude..:? "PiiEntityTypes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RedactionConfig

instance Prelude.NFData RedactionConfig

instance Prelude.ToJSON RedactionConfig where
  toJSON RedactionConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaskCharacter" Prelude..=)
              Prelude.<$> maskCharacter,
            ("MaskMode" Prelude..=) Prelude.<$> maskMode,
            ("PiiEntityTypes" Prelude..=)
              Prelude.<$> piiEntityTypes
          ]
      )
