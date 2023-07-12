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
-- Module      : Amazonka.ComputeOptimizer.Types.ExternalMetricsPreference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExternalMetricsPreference where

import Amazonka.ComputeOptimizer.Types.ExternalMetricsSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the external metrics preferences for EC2 rightsizing
-- recommendations.
--
-- /See:/ 'newExternalMetricsPreference' smart constructor.
data ExternalMetricsPreference = ExternalMetricsPreference'
  { -- | Contains the source options for external metrics preferences.
    source :: Prelude.Maybe ExternalMetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalMetricsPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'externalMetricsPreference_source' - Contains the source options for external metrics preferences.
newExternalMetricsPreference ::
  ExternalMetricsPreference
newExternalMetricsPreference =
  ExternalMetricsPreference'
    { source =
        Prelude.Nothing
    }

-- | Contains the source options for external metrics preferences.
externalMetricsPreference_source :: Lens.Lens' ExternalMetricsPreference (Prelude.Maybe ExternalMetricsSource)
externalMetricsPreference_source = Lens.lens (\ExternalMetricsPreference' {source} -> source) (\s@ExternalMetricsPreference' {} a -> s {source = a} :: ExternalMetricsPreference)

instance Data.FromJSON ExternalMetricsPreference where
  parseJSON =
    Data.withObject
      "ExternalMetricsPreference"
      ( \x ->
          ExternalMetricsPreference'
            Prelude.<$> (x Data..:? "source")
      )

instance Prelude.Hashable ExternalMetricsPreference where
  hashWithSalt _salt ExternalMetricsPreference' {..} =
    _salt `Prelude.hashWithSalt` source

instance Prelude.NFData ExternalMetricsPreference where
  rnf ExternalMetricsPreference' {..} =
    Prelude.rnf source

instance Data.ToJSON ExternalMetricsPreference where
  toJSON ExternalMetricsPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [("source" Data..=) Prelude.<$> source]
      )
