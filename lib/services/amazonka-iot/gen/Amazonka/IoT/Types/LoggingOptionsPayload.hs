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
-- Module      : Amazonka.IoT.Types.LoggingOptionsPayload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.LoggingOptionsPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.LogLevel
import qualified Amazonka.Prelude as Prelude

-- | Describes the logging options payload.
--
-- /See:/ 'newLoggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
  { -- | The log level.
    logLevel :: Prelude.Maybe LogLevel,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  LoggingOptionsPayload
newLoggingOptionsPayload pRoleArn_ =
  LoggingOptionsPayload'
    { logLevel = Prelude.Nothing,
      roleArn = pRoleArn_
    }

-- | The log level.
loggingOptionsPayload_logLevel :: Lens.Lens' LoggingOptionsPayload (Prelude.Maybe LogLevel)
loggingOptionsPayload_logLevel = Lens.lens (\LoggingOptionsPayload' {logLevel} -> logLevel) (\s@LoggingOptionsPayload' {} a -> s {logLevel = a} :: LoggingOptionsPayload)

-- | The ARN of the IAM role that grants access.
loggingOptionsPayload_roleArn :: Lens.Lens' LoggingOptionsPayload Prelude.Text
loggingOptionsPayload_roleArn = Lens.lens (\LoggingOptionsPayload' {roleArn} -> roleArn) (\s@LoggingOptionsPayload' {} a -> s {roleArn = a} :: LoggingOptionsPayload)

instance Prelude.Hashable LoggingOptionsPayload where
  hashWithSalt _salt LoggingOptionsPayload' {..} =
    _salt `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData LoggingOptionsPayload where
  rnf LoggingOptionsPayload' {..} =
    Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON LoggingOptionsPayload where
  toJSON LoggingOptionsPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logLevel" Data..=) Prelude.<$> logLevel,
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
