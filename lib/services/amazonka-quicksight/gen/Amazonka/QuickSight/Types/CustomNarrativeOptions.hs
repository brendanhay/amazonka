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
-- Module      : Amazonka.QuickSight.Types.CustomNarrativeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomNarrativeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The custom narrative options.
--
-- /See:/ 'newCustomNarrativeOptions' smart constructor.
data CustomNarrativeOptions = CustomNarrativeOptions'
  { -- | The string input of custom narrative.
    narrative :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomNarrativeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'narrative', 'customNarrativeOptions_narrative' - The string input of custom narrative.
newCustomNarrativeOptions ::
  -- | 'narrative'
  Prelude.Text ->
  CustomNarrativeOptions
newCustomNarrativeOptions pNarrative_ =
  CustomNarrativeOptions' {narrative = pNarrative_}

-- | The string input of custom narrative.
customNarrativeOptions_narrative :: Lens.Lens' CustomNarrativeOptions Prelude.Text
customNarrativeOptions_narrative = Lens.lens (\CustomNarrativeOptions' {narrative} -> narrative) (\s@CustomNarrativeOptions' {} a -> s {narrative = a} :: CustomNarrativeOptions)

instance Data.FromJSON CustomNarrativeOptions where
  parseJSON =
    Data.withObject
      "CustomNarrativeOptions"
      ( \x ->
          CustomNarrativeOptions'
            Prelude.<$> (x Data..: "Narrative")
      )

instance Prelude.Hashable CustomNarrativeOptions where
  hashWithSalt _salt CustomNarrativeOptions' {..} =
    _salt `Prelude.hashWithSalt` narrative

instance Prelude.NFData CustomNarrativeOptions where
  rnf CustomNarrativeOptions' {..} =
    Prelude.rnf narrative

instance Data.ToJSON CustomNarrativeOptions where
  toJSON CustomNarrativeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Narrative" Data..= narrative)]
      )
