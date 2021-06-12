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
-- Module      : Network.AWS.IoTAnalytics.Types.LoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LoggingOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.LoggingLevel
import qualified Network.AWS.Lens as Lens

-- | Information about logging options.
--
-- /See:/ 'newLoggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { -- | The ARN of the role that grants permission to AWS IoT Analytics to
    -- perform logging.
    roleArn :: Core.Text,
    -- | The logging level. Currently, only ERROR is supported.
    level :: LoggingLevel,
    -- | If true, logging is enabled for AWS IoT Analytics.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'loggingOptions_roleArn' - The ARN of the role that grants permission to AWS IoT Analytics to
-- perform logging.
--
-- 'level', 'loggingOptions_level' - The logging level. Currently, only ERROR is supported.
--
-- 'enabled', 'loggingOptions_enabled' - If true, logging is enabled for AWS IoT Analytics.
newLoggingOptions ::
  -- | 'roleArn'
  Core.Text ->
  -- | 'level'
  LoggingLevel ->
  -- | 'enabled'
  Core.Bool ->
  LoggingOptions
newLoggingOptions pRoleArn_ pLevel_ pEnabled_ =
  LoggingOptions'
    { roleArn = pRoleArn_,
      level = pLevel_,
      enabled = pEnabled_
    }

-- | The ARN of the role that grants permission to AWS IoT Analytics to
-- perform logging.
loggingOptions_roleArn :: Lens.Lens' LoggingOptions Core.Text
loggingOptions_roleArn = Lens.lens (\LoggingOptions' {roleArn} -> roleArn) (\s@LoggingOptions' {} a -> s {roleArn = a} :: LoggingOptions)

-- | The logging level. Currently, only ERROR is supported.
loggingOptions_level :: Lens.Lens' LoggingOptions LoggingLevel
loggingOptions_level = Lens.lens (\LoggingOptions' {level} -> level) (\s@LoggingOptions' {} a -> s {level = a} :: LoggingOptions)

-- | If true, logging is enabled for AWS IoT Analytics.
loggingOptions_enabled :: Lens.Lens' LoggingOptions Core.Bool
loggingOptions_enabled = Lens.lens (\LoggingOptions' {enabled} -> enabled) (\s@LoggingOptions' {} a -> s {enabled = a} :: LoggingOptions)

instance Core.FromJSON LoggingOptions where
  parseJSON =
    Core.withObject
      "LoggingOptions"
      ( \x ->
          LoggingOptions'
            Core.<$> (x Core..: "roleArn")
            Core.<*> (x Core..: "level")
            Core.<*> (x Core..: "enabled")
      )

instance Core.Hashable LoggingOptions

instance Core.NFData LoggingOptions

instance Core.ToJSON LoggingOptions where
  toJSON LoggingOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("level" Core..= level),
            Core.Just ("enabled" Core..= enabled)
          ]
      )
