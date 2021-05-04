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
-- Module      : Network.AWS.IoT.Types.LogTargetConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTargetConfiguration where

import Network.AWS.IoT.Types.LogLevel
import Network.AWS.IoT.Types.LogTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The target configuration.
--
-- /See:/ 'newLogTargetConfiguration' smart constructor.
data LogTargetConfiguration = LogTargetConfiguration'
  { -- | The logging level.
    logLevel :: Prelude.Maybe LogLevel,
    -- | A log target
    logTarget :: Prelude.Maybe LogTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LogTargetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'logTargetConfiguration_logLevel' - The logging level.
--
-- 'logTarget', 'logTargetConfiguration_logTarget' - A log target
newLogTargetConfiguration ::
  LogTargetConfiguration
newLogTargetConfiguration =
  LogTargetConfiguration'
    { logLevel = Prelude.Nothing,
      logTarget = Prelude.Nothing
    }

-- | The logging level.
logTargetConfiguration_logLevel :: Lens.Lens' LogTargetConfiguration (Prelude.Maybe LogLevel)
logTargetConfiguration_logLevel = Lens.lens (\LogTargetConfiguration' {logLevel} -> logLevel) (\s@LogTargetConfiguration' {} a -> s {logLevel = a} :: LogTargetConfiguration)

-- | A log target
logTargetConfiguration_logTarget :: Lens.Lens' LogTargetConfiguration (Prelude.Maybe LogTarget)
logTargetConfiguration_logTarget = Lens.lens (\LogTargetConfiguration' {logTarget} -> logTarget) (\s@LogTargetConfiguration' {} a -> s {logTarget = a} :: LogTargetConfiguration)

instance Prelude.FromJSON LogTargetConfiguration where
  parseJSON =
    Prelude.withObject
      "LogTargetConfiguration"
      ( \x ->
          LogTargetConfiguration'
            Prelude.<$> (x Prelude..:? "logLevel")
            Prelude.<*> (x Prelude..:? "logTarget")
      )

instance Prelude.Hashable LogTargetConfiguration

instance Prelude.NFData LogTargetConfiguration
