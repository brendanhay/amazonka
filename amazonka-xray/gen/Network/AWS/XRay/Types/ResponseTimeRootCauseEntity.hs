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
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseEntity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A collection of segments and corresponding subsegments associated to a
-- response time warning.
--
-- /See:/ 'newResponseTimeRootCauseEntity' smart constructor.
data ResponseTimeRootCauseEntity = ResponseTimeRootCauseEntity'
  { -- | A flag that denotes a remote subsegment.
    remote :: Core.Maybe Core.Bool,
    -- | The name of the entity.
    name :: Core.Maybe Core.Text,
    -- | The type and messages of the exceptions.
    coverage :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'name', 'responseTimeRootCauseEntity_name' - The name of the entity.
--
-- 'coverage', 'responseTimeRootCauseEntity_coverage' - The type and messages of the exceptions.
newResponseTimeRootCauseEntity ::
  ResponseTimeRootCauseEntity
newResponseTimeRootCauseEntity =
  ResponseTimeRootCauseEntity'
    { remote = Core.Nothing,
      name = Core.Nothing,
      coverage = Core.Nothing
    }

-- | A flag that denotes a remote subsegment.
responseTimeRootCauseEntity_remote :: Lens.Lens' ResponseTimeRootCauseEntity (Core.Maybe Core.Bool)
responseTimeRootCauseEntity_remote = Lens.lens (\ResponseTimeRootCauseEntity' {remote} -> remote) (\s@ResponseTimeRootCauseEntity' {} a -> s {remote = a} :: ResponseTimeRootCauseEntity)

-- | The name of the entity.
responseTimeRootCauseEntity_name :: Lens.Lens' ResponseTimeRootCauseEntity (Core.Maybe Core.Text)
responseTimeRootCauseEntity_name = Lens.lens (\ResponseTimeRootCauseEntity' {name} -> name) (\s@ResponseTimeRootCauseEntity' {} a -> s {name = a} :: ResponseTimeRootCauseEntity)

-- | The type and messages of the exceptions.
responseTimeRootCauseEntity_coverage :: Lens.Lens' ResponseTimeRootCauseEntity (Core.Maybe Core.Double)
responseTimeRootCauseEntity_coverage = Lens.lens (\ResponseTimeRootCauseEntity' {coverage} -> coverage) (\s@ResponseTimeRootCauseEntity' {} a -> s {coverage = a} :: ResponseTimeRootCauseEntity)

instance Core.FromJSON ResponseTimeRootCauseEntity where
  parseJSON =
    Core.withObject
      "ResponseTimeRootCauseEntity"
      ( \x ->
          ResponseTimeRootCauseEntity'
            Core.<$> (x Core..:? "Remote")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Coverage")
      )

instance Core.Hashable ResponseTimeRootCauseEntity

instance Core.NFData ResponseTimeRootCauseEntity
