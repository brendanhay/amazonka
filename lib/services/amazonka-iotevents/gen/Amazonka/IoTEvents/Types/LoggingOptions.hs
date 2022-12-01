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
-- Module      : Amazonka.IoTEvents.Types.LoggingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.LoggingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.DetectorDebugOption
import Amazonka.IoTEvents.Types.LoggingLevel
import qualified Amazonka.Prelude as Prelude

-- | The values of the AWS IoT Events logging options.
--
-- /See:/ 'newLoggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { -- | Information that identifies those detector models and their detectors
    -- (instances) for which the logging level is given.
    detectorDebugOptions :: Prelude.Maybe (Prelude.NonEmpty DetectorDebugOption),
    -- | The ARN of the role that grants permission to AWS IoT Events to perform
    -- logging.
    roleArn :: Prelude.Text,
    -- | The logging level.
    level :: LoggingLevel,
    -- | If TRUE, logging is enabled for AWS IoT Events.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorDebugOptions', 'loggingOptions_detectorDebugOptions' - Information that identifies those detector models and their detectors
-- (instances) for which the logging level is given.
--
-- 'roleArn', 'loggingOptions_roleArn' - The ARN of the role that grants permission to AWS IoT Events to perform
-- logging.
--
-- 'level', 'loggingOptions_level' - The logging level.
--
-- 'enabled', 'loggingOptions_enabled' - If TRUE, logging is enabled for AWS IoT Events.
newLoggingOptions ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'level'
  LoggingLevel ->
  -- | 'enabled'
  Prelude.Bool ->
  LoggingOptions
newLoggingOptions pRoleArn_ pLevel_ pEnabled_ =
  LoggingOptions'
    { detectorDebugOptions =
        Prelude.Nothing,
      roleArn = pRoleArn_,
      level = pLevel_,
      enabled = pEnabled_
    }

-- | Information that identifies those detector models and their detectors
-- (instances) for which the logging level is given.
loggingOptions_detectorDebugOptions :: Lens.Lens' LoggingOptions (Prelude.Maybe (Prelude.NonEmpty DetectorDebugOption))
loggingOptions_detectorDebugOptions = Lens.lens (\LoggingOptions' {detectorDebugOptions} -> detectorDebugOptions) (\s@LoggingOptions' {} a -> s {detectorDebugOptions = a} :: LoggingOptions) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the role that grants permission to AWS IoT Events to perform
-- logging.
loggingOptions_roleArn :: Lens.Lens' LoggingOptions Prelude.Text
loggingOptions_roleArn = Lens.lens (\LoggingOptions' {roleArn} -> roleArn) (\s@LoggingOptions' {} a -> s {roleArn = a} :: LoggingOptions)

-- | The logging level.
loggingOptions_level :: Lens.Lens' LoggingOptions LoggingLevel
loggingOptions_level = Lens.lens (\LoggingOptions' {level} -> level) (\s@LoggingOptions' {} a -> s {level = a} :: LoggingOptions)

-- | If TRUE, logging is enabled for AWS IoT Events.
loggingOptions_enabled :: Lens.Lens' LoggingOptions Prelude.Bool
loggingOptions_enabled = Lens.lens (\LoggingOptions' {enabled} -> enabled) (\s@LoggingOptions' {} a -> s {enabled = a} :: LoggingOptions)

instance Core.FromJSON LoggingOptions where
  parseJSON =
    Core.withObject
      "LoggingOptions"
      ( \x ->
          LoggingOptions'
            Prelude.<$> (x Core..:? "detectorDebugOptions")
            Prelude.<*> (x Core..: "roleArn")
            Prelude.<*> (x Core..: "level")
            Prelude.<*> (x Core..: "enabled")
      )

instance Prelude.Hashable LoggingOptions where
  hashWithSalt _salt LoggingOptions' {..} =
    _salt `Prelude.hashWithSalt` detectorDebugOptions
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData LoggingOptions where
  rnf LoggingOptions' {..} =
    Prelude.rnf detectorDebugOptions
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON LoggingOptions where
  toJSON LoggingOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("detectorDebugOptions" Core..=)
              Prelude.<$> detectorDebugOptions,
            Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just ("level" Core..= level),
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )
