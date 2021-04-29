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
-- Module      : Network.AWS.Greengrass.Types.LoggerDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerDefinitionVersion where

import Network.AWS.Greengrass.Types.GreengrassLogger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a logger definition version.
--
-- /See:/ 'newLoggerDefinitionVersion' smart constructor.
data LoggerDefinitionVersion = LoggerDefinitionVersion'
  { -- | A list of loggers.
    loggers :: Prelude.Maybe [GreengrassLogger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  LoggerDefinitionVersion' {loggers = Prelude.Nothing}

-- | A list of loggers.
loggerDefinitionVersion_loggers :: Lens.Lens' LoggerDefinitionVersion (Prelude.Maybe [GreengrassLogger])
loggerDefinitionVersion_loggers = Lens.lens (\LoggerDefinitionVersion' {loggers} -> loggers) (\s@LoggerDefinitionVersion' {} a -> s {loggers = a} :: LoggerDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON LoggerDefinitionVersion where
  parseJSON =
    Prelude.withObject
      "LoggerDefinitionVersion"
      ( \x ->
          LoggerDefinitionVersion'
            Prelude.<$> (x Prelude..:? "Loggers" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable LoggerDefinitionVersion

instance Prelude.NFData LoggerDefinitionVersion

instance Prelude.ToJSON LoggerDefinitionVersion where
  toJSON LoggerDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Loggers" Prelude..=) Prelude.<$> loggers]
      )
