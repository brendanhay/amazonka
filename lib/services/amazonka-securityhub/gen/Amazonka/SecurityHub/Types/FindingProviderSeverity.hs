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
-- Module      : Amazonka.SecurityHub.Types.FindingProviderSeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingProviderSeverity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.SeverityLabel

-- | The severity assigned to the finding by the finding provider.
--
-- /See:/ 'newFindingProviderSeverity' smart constructor.
data FindingProviderSeverity = FindingProviderSeverity'
  { -- | The severity label assigned to the finding by the finding provider.
    label :: Prelude.Maybe SeverityLabel,
    -- | The finding provider\'s original value for the severity.
    original :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingProviderSeverity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'findingProviderSeverity_label' - The severity label assigned to the finding by the finding provider.
--
-- 'original', 'findingProviderSeverity_original' - The finding provider\'s original value for the severity.
newFindingProviderSeverity ::
  FindingProviderSeverity
newFindingProviderSeverity =
  FindingProviderSeverity'
    { label = Prelude.Nothing,
      original = Prelude.Nothing
    }

-- | The severity label assigned to the finding by the finding provider.
findingProviderSeverity_label :: Lens.Lens' FindingProviderSeverity (Prelude.Maybe SeverityLabel)
findingProviderSeverity_label = Lens.lens (\FindingProviderSeverity' {label} -> label) (\s@FindingProviderSeverity' {} a -> s {label = a} :: FindingProviderSeverity)

-- | The finding provider\'s original value for the severity.
findingProviderSeverity_original :: Lens.Lens' FindingProviderSeverity (Prelude.Maybe Prelude.Text)
findingProviderSeverity_original = Lens.lens (\FindingProviderSeverity' {original} -> original) (\s@FindingProviderSeverity' {} a -> s {original = a} :: FindingProviderSeverity)

instance Data.FromJSON FindingProviderSeverity where
  parseJSON =
    Data.withObject
      "FindingProviderSeverity"
      ( \x ->
          FindingProviderSeverity'
            Prelude.<$> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Original")
      )

instance Prelude.Hashable FindingProviderSeverity where
  hashWithSalt _salt FindingProviderSeverity' {..} =
    _salt
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` original

instance Prelude.NFData FindingProviderSeverity where
  rnf FindingProviderSeverity' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf original

instance Data.ToJSON FindingProviderSeverity where
  toJSON FindingProviderSeverity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Label" Data..=) Prelude.<$> label,
            ("Original" Data..=) Prelude.<$> original
          ]
      )
