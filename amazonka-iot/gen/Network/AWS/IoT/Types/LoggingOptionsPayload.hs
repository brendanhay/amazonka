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
-- Module      : Network.AWS.IoT.Types.LoggingOptionsPayload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LoggingOptionsPayload where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.LogLevel
import qualified Network.AWS.Lens as Lens

-- | Describes the logging options payload.
--
-- /See:/ 'newLoggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
  { -- | The log level.
    logLevel :: Core.Maybe LogLevel,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoggingOptionsPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'loggingOptionsPayload_logLevel' - The log level.
--
-- 'roleArn', 'loggingOptionsPayload_roleArn' - The ARN of the IAM role that grants access.
newLoggingOptionsPayload ::
  -- | 'roleArn'
  Core.Text ->
  LoggingOptionsPayload
newLoggingOptionsPayload pRoleArn_ =
  LoggingOptionsPayload'
    { logLevel = Core.Nothing,
      roleArn = pRoleArn_
    }

-- | The log level.
loggingOptionsPayload_logLevel :: Lens.Lens' LoggingOptionsPayload (Core.Maybe LogLevel)
loggingOptionsPayload_logLevel = Lens.lens (\LoggingOptionsPayload' {logLevel} -> logLevel) (\s@LoggingOptionsPayload' {} a -> s {logLevel = a} :: LoggingOptionsPayload)

-- | The ARN of the IAM role that grants access.
loggingOptionsPayload_roleArn :: Lens.Lens' LoggingOptionsPayload Core.Text
loggingOptionsPayload_roleArn = Lens.lens (\LoggingOptionsPayload' {roleArn} -> roleArn) (\s@LoggingOptionsPayload' {} a -> s {roleArn = a} :: LoggingOptionsPayload)

instance Core.Hashable LoggingOptionsPayload

instance Core.NFData LoggingOptionsPayload

instance Core.ToJSON LoggingOptionsPayload where
  toJSON LoggingOptionsPayload' {..} =
    Core.object
      ( Core.catMaybes
          [ ("logLevel" Core..=) Core.<$> logLevel,
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
