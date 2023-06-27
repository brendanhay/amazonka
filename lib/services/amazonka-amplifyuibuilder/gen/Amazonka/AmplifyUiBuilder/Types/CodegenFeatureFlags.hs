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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenFeatureFlags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenFeatureFlags where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the feature flags that you can specify for a code generation
-- job.
--
-- /See:/ 'newCodegenFeatureFlags' smart constructor.
data CodegenFeatureFlags = CodegenFeatureFlags'
  { -- | Specifies whether a code generation job supports non models.
    isNonModelSupported :: Prelude.Maybe Prelude.Bool,
    -- | Specifes whether a code generation job supports data relationships.
    isRelationshipSupported :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenFeatureFlags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isNonModelSupported', 'codegenFeatureFlags_isNonModelSupported' - Specifies whether a code generation job supports non models.
--
-- 'isRelationshipSupported', 'codegenFeatureFlags_isRelationshipSupported' - Specifes whether a code generation job supports data relationships.
newCodegenFeatureFlags ::
  CodegenFeatureFlags
newCodegenFeatureFlags =
  CodegenFeatureFlags'
    { isNonModelSupported =
        Prelude.Nothing,
      isRelationshipSupported = Prelude.Nothing
    }

-- | Specifies whether a code generation job supports non models.
codegenFeatureFlags_isNonModelSupported :: Lens.Lens' CodegenFeatureFlags (Prelude.Maybe Prelude.Bool)
codegenFeatureFlags_isNonModelSupported = Lens.lens (\CodegenFeatureFlags' {isNonModelSupported} -> isNonModelSupported) (\s@CodegenFeatureFlags' {} a -> s {isNonModelSupported = a} :: CodegenFeatureFlags)

-- | Specifes whether a code generation job supports data relationships.
codegenFeatureFlags_isRelationshipSupported :: Lens.Lens' CodegenFeatureFlags (Prelude.Maybe Prelude.Bool)
codegenFeatureFlags_isRelationshipSupported = Lens.lens (\CodegenFeatureFlags' {isRelationshipSupported} -> isRelationshipSupported) (\s@CodegenFeatureFlags' {} a -> s {isRelationshipSupported = a} :: CodegenFeatureFlags)

instance Data.FromJSON CodegenFeatureFlags where
  parseJSON =
    Data.withObject
      "CodegenFeatureFlags"
      ( \x ->
          CodegenFeatureFlags'
            Prelude.<$> (x Data..:? "isNonModelSupported")
            Prelude.<*> (x Data..:? "isRelationshipSupported")
      )

instance Prelude.Hashable CodegenFeatureFlags where
  hashWithSalt _salt CodegenFeatureFlags' {..} =
    _salt
      `Prelude.hashWithSalt` isNonModelSupported
      `Prelude.hashWithSalt` isRelationshipSupported

instance Prelude.NFData CodegenFeatureFlags where
  rnf CodegenFeatureFlags' {..} =
    Prelude.rnf isNonModelSupported
      `Prelude.seq` Prelude.rnf isRelationshipSupported

instance Data.ToJSON CodegenFeatureFlags where
  toJSON CodegenFeatureFlags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isNonModelSupported" Data..=)
              Prelude.<$> isNonModelSupported,
            ("isRelationshipSupported" Data..=)
              Prelude.<$> isRelationshipSupported
          ]
      )
