{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types.LogTargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A log target.
--
-- /See:/ 'newLogTarget' smart constructor.
data LogTarget = LogTarget'
  { -- | The target name.
    targetName :: Prelude.Maybe Prelude.Text,
    -- | The target type.
    targetType :: LogTargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { targetName = Prelude.Nothing,
      targetType = pTargetType_
    }

-- | The target name.
logTarget_targetName :: Lens.Lens' LogTarget (Prelude.Maybe Prelude.Text)
logTarget_targetName = Lens.lens (\LogTarget' {targetName} -> targetName) (\s@LogTarget' {} a -> s {targetName = a} :: LogTarget)

-- | The target type.
logTarget_targetType :: Lens.Lens' LogTarget LogTargetType
logTarget_targetType = Lens.lens (\LogTarget' {targetType} -> targetType) (\s@LogTarget' {} a -> s {targetType = a} :: LogTarget)

instance Prelude.FromJSON LogTarget where
  parseJSON =
    Prelude.withObject
      "LogTarget"
      ( \x ->
          LogTarget'
            Prelude.<$> (x Prelude..:? "targetName")
            Prelude.<*> (x Prelude..: "targetType")
      )

instance Prelude.Hashable LogTarget

instance Prelude.NFData LogTarget

instance Prelude.ToJSON LogTarget where
  toJSON LogTarget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("targetName" Prelude..=) Prelude.<$> targetName,
            Prelude.Just ("targetType" Prelude..= targetType)
          ]
      )
