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
-- Module      : Network.AWS.IoT.Types.LogTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTarget where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.LogTargetType
import qualified Network.AWS.Lens as Lens

-- | A log target.
--
-- /See:/ 'newLogTarget' smart constructor.
data LogTarget = LogTarget'
  { -- | The target name.
    targetName :: Core.Maybe Core.Text,
    -- | The target type.
    targetType :: LogTargetType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetName', 'logTarget_targetName' - The target name.
--
-- 'targetType', 'logTarget_targetType' - The target type.
newLogTarget ::
  -- | 'targetType'
  LogTargetType ->
  LogTarget
newLogTarget pTargetType_ =
  LogTarget'
    { targetName = Core.Nothing,
      targetType = pTargetType_
    }

-- | The target name.
logTarget_targetName :: Lens.Lens' LogTarget (Core.Maybe Core.Text)
logTarget_targetName = Lens.lens (\LogTarget' {targetName} -> targetName) (\s@LogTarget' {} a -> s {targetName = a} :: LogTarget)

-- | The target type.
logTarget_targetType :: Lens.Lens' LogTarget LogTargetType
logTarget_targetType = Lens.lens (\LogTarget' {targetType} -> targetType) (\s@LogTarget' {} a -> s {targetType = a} :: LogTarget)

instance Core.FromJSON LogTarget where
  parseJSON =
    Core.withObject
      "LogTarget"
      ( \x ->
          LogTarget'
            Core.<$> (x Core..:? "targetName")
            Core.<*> (x Core..: "targetType")
      )

instance Core.Hashable LogTarget

instance Core.NFData LogTarget

instance Core.ToJSON LogTarget where
  toJSON LogTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetName" Core..=) Core.<$> targetName,
            Core.Just ("targetType" Core..= targetType)
          ]
      )
