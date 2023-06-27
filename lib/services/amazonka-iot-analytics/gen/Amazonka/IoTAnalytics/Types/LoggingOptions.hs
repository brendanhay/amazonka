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
-- Module      : Amazonka.IoTAnalytics.Types.LoggingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.LoggingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.LoggingLevel
import qualified Amazonka.Prelude as Prelude

-- | Information about logging options.
--
-- /See:/ 'newLoggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { -- | The ARN of the role that grants permission to IoT Analytics to perform
    -- logging.
    roleArn :: Prelude.Text,
    -- | The logging level. Currently, only ERROR is supported.
    level :: LoggingLevel,
    -- | If true, logging is enabled for IoT Analytics.
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
-- 'roleArn', 'loggingOptions_roleArn' - The ARN of the role that grants permission to IoT Analytics to perform
-- logging.
--
-- 'level', 'loggingOptions_level' - The logging level. Currently, only ERROR is supported.
--
-- 'enabled', 'loggingOptions_enabled' - If true, logging is enabled for IoT Analytics.
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
    { roleArn = pRoleArn_,
      level = pLevel_,
      enabled = pEnabled_
    }

-- | The ARN of the role that grants permission to IoT Analytics to perform
-- logging.
loggingOptions_roleArn :: Lens.Lens' LoggingOptions Prelude.Text
loggingOptions_roleArn = Lens.lens (\LoggingOptions' {roleArn} -> roleArn) (\s@LoggingOptions' {} a -> s {roleArn = a} :: LoggingOptions)

-- | The logging level. Currently, only ERROR is supported.
loggingOptions_level :: Lens.Lens' LoggingOptions LoggingLevel
loggingOptions_level = Lens.lens (\LoggingOptions' {level} -> level) (\s@LoggingOptions' {} a -> s {level = a} :: LoggingOptions)

-- | If true, logging is enabled for IoT Analytics.
loggingOptions_enabled :: Lens.Lens' LoggingOptions Prelude.Bool
loggingOptions_enabled = Lens.lens (\LoggingOptions' {enabled} -> enabled) (\s@LoggingOptions' {} a -> s {enabled = a} :: LoggingOptions)

instance Data.FromJSON LoggingOptions where
  parseJSON =
    Data.withObject
      "LoggingOptions"
      ( \x ->
          LoggingOptions'
            Prelude.<$> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "level")
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable LoggingOptions where
  hashWithSalt _salt LoggingOptions' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData LoggingOptions where
  rnf LoggingOptions' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON LoggingOptions where
  toJSON LoggingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("level" Data..= level),
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
