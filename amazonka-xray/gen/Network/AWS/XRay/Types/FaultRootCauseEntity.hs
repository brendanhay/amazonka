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
-- Module      : Network.AWS.XRay.Types.FaultRootCauseEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseEntity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a
-- trace summary fault error.
--
-- /See:/ 'newFaultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Core.Maybe [RootCauseException],
    -- | A flag that denotes a remote subsegment.
    remote :: Core.Maybe Core.Bool,
    -- | The name of the entity.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FaultRootCauseEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptions', 'faultRootCauseEntity_exceptions' - The types and messages of the exceptions.
--
-- 'remote', 'faultRootCauseEntity_remote' - A flag that denotes a remote subsegment.
--
-- 'name', 'faultRootCauseEntity_name' - The name of the entity.
newFaultRootCauseEntity ::
  FaultRootCauseEntity
newFaultRootCauseEntity =
  FaultRootCauseEntity'
    { exceptions = Core.Nothing,
      remote = Core.Nothing,
      name = Core.Nothing
    }

-- | The types and messages of the exceptions.
faultRootCauseEntity_exceptions :: Lens.Lens' FaultRootCauseEntity (Core.Maybe [RootCauseException])
faultRootCauseEntity_exceptions = Lens.lens (\FaultRootCauseEntity' {exceptions} -> exceptions) (\s@FaultRootCauseEntity' {} a -> s {exceptions = a} :: FaultRootCauseEntity) Core.. Lens.mapping Lens._Coerce

-- | A flag that denotes a remote subsegment.
faultRootCauseEntity_remote :: Lens.Lens' FaultRootCauseEntity (Core.Maybe Core.Bool)
faultRootCauseEntity_remote = Lens.lens (\FaultRootCauseEntity' {remote} -> remote) (\s@FaultRootCauseEntity' {} a -> s {remote = a} :: FaultRootCauseEntity)

-- | The name of the entity.
faultRootCauseEntity_name :: Lens.Lens' FaultRootCauseEntity (Core.Maybe Core.Text)
faultRootCauseEntity_name = Lens.lens (\FaultRootCauseEntity' {name} -> name) (\s@FaultRootCauseEntity' {} a -> s {name = a} :: FaultRootCauseEntity)

instance Core.FromJSON FaultRootCauseEntity where
  parseJSON =
    Core.withObject
      "FaultRootCauseEntity"
      ( \x ->
          FaultRootCauseEntity'
            Core.<$> (x Core..:? "Exceptions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Remote")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable FaultRootCauseEntity

instance Core.NFData FaultRootCauseEntity
