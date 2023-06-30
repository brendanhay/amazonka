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
-- Module      : Amazonka.IoT.Types.EnableIoTLoggingParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.EnableIoTLoggingParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.LogLevel
import qualified Amazonka.Prelude as Prelude

-- | Parameters used when defining a mitigation action that enable Amazon Web
-- Services IoT Core logging.
--
-- /See:/ 'newEnableIoTLoggingParams' smart constructor.
data EnableIoTLoggingParams = EnableIoTLoggingParams'
  { -- | The Amazon Resource Name (ARN) of the IAM role used for logging.
    roleArnForLogging :: Prelude.Text,
    -- | Specifies the type of information to be logged.
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableIoTLoggingParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArnForLogging', 'enableIoTLoggingParams_roleArnForLogging' - The Amazon Resource Name (ARN) of the IAM role used for logging.
--
-- 'logLevel', 'enableIoTLoggingParams_logLevel' - Specifies the type of information to be logged.
newEnableIoTLoggingParams ::
  -- | 'roleArnForLogging'
  Prelude.Text ->
  -- | 'logLevel'
  LogLevel ->
  EnableIoTLoggingParams
newEnableIoTLoggingParams
  pRoleArnForLogging_
  pLogLevel_ =
    EnableIoTLoggingParams'
      { roleArnForLogging =
          pRoleArnForLogging_,
        logLevel = pLogLevel_
      }

-- | The Amazon Resource Name (ARN) of the IAM role used for logging.
enableIoTLoggingParams_roleArnForLogging :: Lens.Lens' EnableIoTLoggingParams Prelude.Text
enableIoTLoggingParams_roleArnForLogging = Lens.lens (\EnableIoTLoggingParams' {roleArnForLogging} -> roleArnForLogging) (\s@EnableIoTLoggingParams' {} a -> s {roleArnForLogging = a} :: EnableIoTLoggingParams)

-- | Specifies the type of information to be logged.
enableIoTLoggingParams_logLevel :: Lens.Lens' EnableIoTLoggingParams LogLevel
enableIoTLoggingParams_logLevel = Lens.lens (\EnableIoTLoggingParams' {logLevel} -> logLevel) (\s@EnableIoTLoggingParams' {} a -> s {logLevel = a} :: EnableIoTLoggingParams)

instance Data.FromJSON EnableIoTLoggingParams where
  parseJSON =
    Data.withObject
      "EnableIoTLoggingParams"
      ( \x ->
          EnableIoTLoggingParams'
            Prelude.<$> (x Data..: "roleArnForLogging")
            Prelude.<*> (x Data..: "logLevel")
      )

instance Prelude.Hashable EnableIoTLoggingParams where
  hashWithSalt _salt EnableIoTLoggingParams' {..} =
    _salt
      `Prelude.hashWithSalt` roleArnForLogging
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData EnableIoTLoggingParams where
  rnf EnableIoTLoggingParams' {..} =
    Prelude.rnf roleArnForLogging
      `Prelude.seq` Prelude.rnf logLevel

instance Data.ToJSON EnableIoTLoggingParams where
  toJSON EnableIoTLoggingParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("roleArnForLogging" Data..= roleArnForLogging),
            Prelude.Just ("logLevel" Data..= logLevel)
          ]
      )
