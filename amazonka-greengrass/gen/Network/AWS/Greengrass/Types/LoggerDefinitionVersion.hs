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
-- Module      : Network.AWS.Greengrass.Types.LoggerDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerDefinitionVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.GreengrassLogger
import qualified Network.AWS.Lens as Lens

-- | Information about a logger definition version.
--
-- /See:/ 'newLoggerDefinitionVersion' smart constructor.
data LoggerDefinitionVersion = LoggerDefinitionVersion'
  { -- | A list of loggers.
    loggers :: Core.Maybe [GreengrassLogger]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoggerDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggers', 'loggerDefinitionVersion_loggers' - A list of loggers.
newLoggerDefinitionVersion ::
  LoggerDefinitionVersion
newLoggerDefinitionVersion =
  LoggerDefinitionVersion' {loggers = Core.Nothing}

-- | A list of loggers.
loggerDefinitionVersion_loggers :: Lens.Lens' LoggerDefinitionVersion (Core.Maybe [GreengrassLogger])
loggerDefinitionVersion_loggers = Lens.lens (\LoggerDefinitionVersion' {loggers} -> loggers) (\s@LoggerDefinitionVersion' {} a -> s {loggers = a} :: LoggerDefinitionVersion) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON LoggerDefinitionVersion where
  parseJSON =
    Core.withObject
      "LoggerDefinitionVersion"
      ( \x ->
          LoggerDefinitionVersion'
            Core.<$> (x Core..:? "Loggers" Core..!= Core.mempty)
      )

instance Core.Hashable LoggerDefinitionVersion

instance Core.NFData LoggerDefinitionVersion

instance Core.ToJSON LoggerDefinitionVersion where
  toJSON LoggerDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Loggers" Core..=) Core.<$> loggers]
      )
