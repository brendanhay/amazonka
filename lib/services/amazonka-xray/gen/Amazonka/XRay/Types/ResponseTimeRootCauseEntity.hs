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
-- Module      : Amazonka.XRay.Types.ResponseTimeRootCauseEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ResponseTimeRootCauseEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of segments and corresponding subsegments associated to a
-- response time warning.
--
-- /See:/ 'newResponseTimeRootCauseEntity' smart constructor.
data ResponseTimeRootCauseEntity = ResponseTimeRootCauseEntity'
  { -- | A flag that denotes a remote subsegment.
    remote :: Prelude.Maybe Prelude.Bool,
    -- | The type and messages of the exceptions.
    coverage :: Prelude.Maybe Prelude.Double,
    -- | The name of the entity.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseTimeRootCauseEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remote', 'responseTimeRootCauseEntity_remote' - A flag that denotes a remote subsegment.
--
-- 'coverage', 'responseTimeRootCauseEntity_coverage' - The type and messages of the exceptions.
--
-- 'name', 'responseTimeRootCauseEntity_name' - The name of the entity.
newResponseTimeRootCauseEntity ::
  ResponseTimeRootCauseEntity
newResponseTimeRootCauseEntity =
  ResponseTimeRootCauseEntity'
    { remote =
        Prelude.Nothing,
      coverage = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A flag that denotes a remote subsegment.
responseTimeRootCauseEntity_remote :: Lens.Lens' ResponseTimeRootCauseEntity (Prelude.Maybe Prelude.Bool)
responseTimeRootCauseEntity_remote = Lens.lens (\ResponseTimeRootCauseEntity' {remote} -> remote) (\s@ResponseTimeRootCauseEntity' {} a -> s {remote = a} :: ResponseTimeRootCauseEntity)

-- | The type and messages of the exceptions.
responseTimeRootCauseEntity_coverage :: Lens.Lens' ResponseTimeRootCauseEntity (Prelude.Maybe Prelude.Double)
responseTimeRootCauseEntity_coverage = Lens.lens (\ResponseTimeRootCauseEntity' {coverage} -> coverage) (\s@ResponseTimeRootCauseEntity' {} a -> s {coverage = a} :: ResponseTimeRootCauseEntity)

-- | The name of the entity.
responseTimeRootCauseEntity_name :: Lens.Lens' ResponseTimeRootCauseEntity (Prelude.Maybe Prelude.Text)
responseTimeRootCauseEntity_name = Lens.lens (\ResponseTimeRootCauseEntity' {name} -> name) (\s@ResponseTimeRootCauseEntity' {} a -> s {name = a} :: ResponseTimeRootCauseEntity)

instance Core.FromJSON ResponseTimeRootCauseEntity where
  parseJSON =
    Core.withObject
      "ResponseTimeRootCauseEntity"
      ( \x ->
          ResponseTimeRootCauseEntity'
            Prelude.<$> (x Core..:? "Remote")
            Prelude.<*> (x Core..:? "Coverage")
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable ResponseTimeRootCauseEntity where
  hashWithSalt _salt ResponseTimeRootCauseEntity' {..} =
    _salt `Prelude.hashWithSalt` remote
      `Prelude.hashWithSalt` coverage
      `Prelude.hashWithSalt` name

instance Prelude.NFData ResponseTimeRootCauseEntity where
  rnf ResponseTimeRootCauseEntity' {..} =
    Prelude.rnf remote
      `Prelude.seq` Prelude.rnf coverage
      `Prelude.seq` Prelude.rnf name
