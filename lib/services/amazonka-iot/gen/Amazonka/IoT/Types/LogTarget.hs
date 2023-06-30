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
-- Module      : Amazonka.IoT.Types.LogTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.LogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.LogTargetType
import qualified Amazonka.Prelude as Prelude

-- | A log target.
--
-- /See:/ 'newLogTarget' smart constructor.
data LogTarget = LogTarget'
  { -- | The target name.
    targetName :: Prelude.Maybe Prelude.Text,
    -- | The target type.
    targetType :: LogTargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON LogTarget where
  parseJSON =
    Data.withObject
      "LogTarget"
      ( \x ->
          LogTarget'
            Prelude.<$> (x Data..:? "targetName")
            Prelude.<*> (x Data..: "targetType")
      )

instance Prelude.Hashable LogTarget where
  hashWithSalt _salt LogTarget' {..} =
    _salt
      `Prelude.hashWithSalt` targetName
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData LogTarget where
  rnf LogTarget' {..} =
    Prelude.rnf targetName
      `Prelude.seq` Prelude.rnf targetType

instance Data.ToJSON LogTarget where
  toJSON LogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("targetName" Data..=) Prelude.<$> targetName,
            Prelude.Just ("targetType" Data..= targetType)
          ]
      )
