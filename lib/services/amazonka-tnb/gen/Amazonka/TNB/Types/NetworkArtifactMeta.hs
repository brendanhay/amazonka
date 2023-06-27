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
-- Module      : Amazonka.TNB.Types.NetworkArtifactMeta
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.NetworkArtifactMeta where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.ToscaOverride

-- | Metadata for network package artifacts.
--
-- Artifacts are the contents of the package descriptor file and the state
-- of the package.
--
-- /See:/ 'newNetworkArtifactMeta' smart constructor.
data NetworkArtifactMeta = NetworkArtifactMeta'
  { -- | Lists network package overrides.
    overrides :: Prelude.Maybe [ToscaOverride]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkArtifactMeta' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'networkArtifactMeta_overrides' - Lists network package overrides.
newNetworkArtifactMeta ::
  NetworkArtifactMeta
newNetworkArtifactMeta =
  NetworkArtifactMeta' {overrides = Prelude.Nothing}

-- | Lists network package overrides.
networkArtifactMeta_overrides :: Lens.Lens' NetworkArtifactMeta (Prelude.Maybe [ToscaOverride])
networkArtifactMeta_overrides = Lens.lens (\NetworkArtifactMeta' {overrides} -> overrides) (\s@NetworkArtifactMeta' {} a -> s {overrides = a} :: NetworkArtifactMeta) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkArtifactMeta where
  parseJSON =
    Data.withObject
      "NetworkArtifactMeta"
      ( \x ->
          NetworkArtifactMeta'
            Prelude.<$> (x Data..:? "overrides" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkArtifactMeta where
  hashWithSalt _salt NetworkArtifactMeta' {..} =
    _salt `Prelude.hashWithSalt` overrides

instance Prelude.NFData NetworkArtifactMeta where
  rnf NetworkArtifactMeta' {..} = Prelude.rnf overrides
