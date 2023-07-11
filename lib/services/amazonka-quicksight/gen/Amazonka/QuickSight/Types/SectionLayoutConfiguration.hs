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
-- Module      : Amazonka.QuickSight.Types.SectionLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FreeFormSectionLayoutConfiguration

-- | The layout configuration of a section.
--
-- /See:/ 'newSectionLayoutConfiguration' smart constructor.
data SectionLayoutConfiguration = SectionLayoutConfiguration'
  { -- | The free-form layout configuration of a section.
    freeFormLayout :: FreeFormSectionLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeFormLayout', 'sectionLayoutConfiguration_freeFormLayout' - The free-form layout configuration of a section.
newSectionLayoutConfiguration ::
  -- | 'freeFormLayout'
  FreeFormSectionLayoutConfiguration ->
  SectionLayoutConfiguration
newSectionLayoutConfiguration pFreeFormLayout_ =
  SectionLayoutConfiguration'
    { freeFormLayout =
        pFreeFormLayout_
    }

-- | The free-form layout configuration of a section.
sectionLayoutConfiguration_freeFormLayout :: Lens.Lens' SectionLayoutConfiguration FreeFormSectionLayoutConfiguration
sectionLayoutConfiguration_freeFormLayout = Lens.lens (\SectionLayoutConfiguration' {freeFormLayout} -> freeFormLayout) (\s@SectionLayoutConfiguration' {} a -> s {freeFormLayout = a} :: SectionLayoutConfiguration)

instance Data.FromJSON SectionLayoutConfiguration where
  parseJSON =
    Data.withObject
      "SectionLayoutConfiguration"
      ( \x ->
          SectionLayoutConfiguration'
            Prelude.<$> (x Data..: "FreeFormLayout")
      )

instance Prelude.Hashable SectionLayoutConfiguration where
  hashWithSalt _salt SectionLayoutConfiguration' {..} =
    _salt `Prelude.hashWithSalt` freeFormLayout

instance Prelude.NFData SectionLayoutConfiguration where
  rnf SectionLayoutConfiguration' {..} =
    Prelude.rnf freeFormLayout

instance Data.ToJSON SectionLayoutConfiguration where
  toJSON SectionLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FreeFormLayout" Data..= freeFormLayout)
          ]
      )
