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
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseEntity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a
-- trace summary error.
--
-- /See:/ 'newErrorRootCauseEntity' smart constructor.
data ErrorRootCauseEntity = ErrorRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Core.Maybe [RootCauseException],
    -- | A flag that denotes a remote subsegment.
    remote :: Core.Maybe Core.Bool,
    -- | The name of the entity.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ErrorRootCauseEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptions', 'errorRootCauseEntity_exceptions' - The types and messages of the exceptions.
--
-- 'remote', 'errorRootCauseEntity_remote' - A flag that denotes a remote subsegment.
--
-- 'name', 'errorRootCauseEntity_name' - The name of the entity.
newErrorRootCauseEntity ::
  ErrorRootCauseEntity
newErrorRootCauseEntity =
  ErrorRootCauseEntity'
    { exceptions = Core.Nothing,
      remote = Core.Nothing,
      name = Core.Nothing
    }

-- | The types and messages of the exceptions.
errorRootCauseEntity_exceptions :: Lens.Lens' ErrorRootCauseEntity (Core.Maybe [RootCauseException])
errorRootCauseEntity_exceptions = Lens.lens (\ErrorRootCauseEntity' {exceptions} -> exceptions) (\s@ErrorRootCauseEntity' {} a -> s {exceptions = a} :: ErrorRootCauseEntity) Core.. Lens.mapping Lens._Coerce

-- | A flag that denotes a remote subsegment.
errorRootCauseEntity_remote :: Lens.Lens' ErrorRootCauseEntity (Core.Maybe Core.Bool)
errorRootCauseEntity_remote = Lens.lens (\ErrorRootCauseEntity' {remote} -> remote) (\s@ErrorRootCauseEntity' {} a -> s {remote = a} :: ErrorRootCauseEntity)

-- | The name of the entity.
errorRootCauseEntity_name :: Lens.Lens' ErrorRootCauseEntity (Core.Maybe Core.Text)
errorRootCauseEntity_name = Lens.lens (\ErrorRootCauseEntity' {name} -> name) (\s@ErrorRootCauseEntity' {} a -> s {name = a} :: ErrorRootCauseEntity)

instance Core.FromJSON ErrorRootCauseEntity where
  parseJSON =
    Core.withObject
      "ErrorRootCauseEntity"
      ( \x ->
          ErrorRootCauseEntity'
            Core.<$> (x Core..:? "Exceptions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Remote")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable ErrorRootCauseEntity

instance Core.NFData ErrorRootCauseEntity
