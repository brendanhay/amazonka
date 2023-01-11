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
-- Module      : Amazonka.Comprehend.Types.RedactionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.RedactionConfig where

import Amazonka.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Amazonka.Comprehend.Types.PiiEntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
redactionConfig_piiEntityTypes = Lens.lens (\RedactionConfig' {piiEntityTypes} -> piiEntityTypes) (\s@RedactionConfig' {} a -> s {piiEntityTypes = a} :: RedactionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RedactionConfig where
  parseJSON =
    Data.withObject
      "RedactionConfig"
      ( \x ->
          RedactionConfig'
            Prelude.<$> (x Data..:? "MaskCharacter")
            Prelude.<*> (x Data..:? "MaskMode")
            Prelude.<*> ( x Data..:? "PiiEntityTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RedactionConfig where
  hashWithSalt _salt RedactionConfig' {..} =
    _salt `Prelude.hashWithSalt` maskCharacter
      `Prelude.hashWithSalt` maskMode
      `Prelude.hashWithSalt` piiEntityTypes

instance Prelude.NFData RedactionConfig where
  rnf RedactionConfig' {..} =
    Prelude.rnf maskCharacter
      `Prelude.seq` Prelude.rnf maskMode
      `Prelude.seq` Prelude.rnf piiEntityTypes

instance Data.ToJSON RedactionConfig where
  toJSON RedactionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaskCharacter" Data..=) Prelude.<$> maskCharacter,
            ("MaskMode" Data..=) Prelude.<$> maskMode,
            ("PiiEntityTypes" Data..=)
              Prelude.<$> piiEntityTypes
          ]
      )
