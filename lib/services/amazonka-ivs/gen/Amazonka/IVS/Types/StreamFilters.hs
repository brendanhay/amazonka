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
-- Module      : Amazonka.IVS.Types.StreamFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.StreamFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.StreamHealth
import qualified Amazonka.Prelude as Prelude

-- | Object specifying the stream attribute on which to filter.
--
-- /See:/ 'newStreamFilters' smart constructor.
data StreamFilters = StreamFilters'
  { -- | The stream’s health.
    health :: Prelude.Maybe StreamHealth
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'health', 'streamFilters_health' - The stream’s health.
newStreamFilters ::
  StreamFilters
newStreamFilters =
  StreamFilters' {health = Prelude.Nothing}

-- | The stream’s health.
streamFilters_health :: Lens.Lens' StreamFilters (Prelude.Maybe StreamHealth)
streamFilters_health = Lens.lens (\StreamFilters' {health} -> health) (\s@StreamFilters' {} a -> s {health = a} :: StreamFilters)

instance Prelude.Hashable StreamFilters where
  hashWithSalt _salt StreamFilters' {..} =
    _salt `Prelude.hashWithSalt` health

instance Prelude.NFData StreamFilters where
  rnf StreamFilters' {..} = Prelude.rnf health

instance Data.ToJSON StreamFilters where
  toJSON StreamFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [("health" Data..=) Prelude.<$> health]
      )
