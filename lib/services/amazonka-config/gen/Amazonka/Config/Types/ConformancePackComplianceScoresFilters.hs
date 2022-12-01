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
-- Module      : Amazonka.Config.Types.ConformancePackComplianceScoresFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackComplianceScoresFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of filters to apply to the conformance pack compliance score
-- result set.
--
-- /See:/ 'newConformancePackComplianceScoresFilters' smart constructor.
data ConformancePackComplianceScoresFilters = ConformancePackComplianceScoresFilters'
  { -- | The names of the conformance packs whose compliance scores you want to
    -- include in the conformance pack compliance score result set. You can
    -- include up to 25 conformance packs in the @ConformancePackNames@ array
    -- of strings, each with a character limit of 256 characters for the
    -- conformance pack name.
    conformancePackNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackComplianceScoresFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackNames', 'conformancePackComplianceScoresFilters_conformancePackNames' - The names of the conformance packs whose compliance scores you want to
-- include in the conformance pack compliance score result set. You can
-- include up to 25 conformance packs in the @ConformancePackNames@ array
-- of strings, each with a character limit of 256 characters for the
-- conformance pack name.
newConformancePackComplianceScoresFilters ::
  -- | 'conformancePackNames'
  Prelude.NonEmpty Prelude.Text ->
  ConformancePackComplianceScoresFilters
newConformancePackComplianceScoresFilters
  pConformancePackNames_ =
    ConformancePackComplianceScoresFilters'
      { conformancePackNames =
          Lens.coerced
            Lens.# pConformancePackNames_
      }

-- | The names of the conformance packs whose compliance scores you want to
-- include in the conformance pack compliance score result set. You can
-- include up to 25 conformance packs in the @ConformancePackNames@ array
-- of strings, each with a character limit of 256 characters for the
-- conformance pack name.
conformancePackComplianceScoresFilters_conformancePackNames :: Lens.Lens' ConformancePackComplianceScoresFilters (Prelude.NonEmpty Prelude.Text)
conformancePackComplianceScoresFilters_conformancePackNames = Lens.lens (\ConformancePackComplianceScoresFilters' {conformancePackNames} -> conformancePackNames) (\s@ConformancePackComplianceScoresFilters' {} a -> s {conformancePackNames = a} :: ConformancePackComplianceScoresFilters) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    ConformancePackComplianceScoresFilters
  where
  hashWithSalt
    _salt
    ConformancePackComplianceScoresFilters' {..} =
      _salt `Prelude.hashWithSalt` conformancePackNames

instance
  Prelude.NFData
    ConformancePackComplianceScoresFilters
  where
  rnf ConformancePackComplianceScoresFilters' {..} =
    Prelude.rnf conformancePackNames

instance
  Core.ToJSON
    ConformancePackComplianceScoresFilters
  where
  toJSON ConformancePackComplianceScoresFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConformancePackNames"
                  Core..= conformancePackNames
              )
          ]
      )
