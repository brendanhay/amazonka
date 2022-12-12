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
-- Module      : Amazonka.AppStream.Types.LastReportGenerationExecutionError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.LastReportGenerationExecutionError where

import Amazonka.AppStream.Types.UsageReportExecutionErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the error that is returned when a usage report can\'t be
-- generated.
--
-- /See:/ 'newLastReportGenerationExecutionError' smart constructor.
data LastReportGenerationExecutionError = LastReportGenerationExecutionError'
  { -- | The error code for the error that is returned when a usage report can\'t
    -- be generated.
    errorCode :: Prelude.Maybe UsageReportExecutionErrorCode,
    -- | The error message for the error that is returned when a usage report
    -- can\'t be generated.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastReportGenerationExecutionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'lastReportGenerationExecutionError_errorCode' - The error code for the error that is returned when a usage report can\'t
-- be generated.
--
-- 'errorMessage', 'lastReportGenerationExecutionError_errorMessage' - The error message for the error that is returned when a usage report
-- can\'t be generated.
newLastReportGenerationExecutionError ::
  LastReportGenerationExecutionError
newLastReportGenerationExecutionError =
  LastReportGenerationExecutionError'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The error code for the error that is returned when a usage report can\'t
-- be generated.
lastReportGenerationExecutionError_errorCode :: Lens.Lens' LastReportGenerationExecutionError (Prelude.Maybe UsageReportExecutionErrorCode)
lastReportGenerationExecutionError_errorCode = Lens.lens (\LastReportGenerationExecutionError' {errorCode} -> errorCode) (\s@LastReportGenerationExecutionError' {} a -> s {errorCode = a} :: LastReportGenerationExecutionError)

-- | The error message for the error that is returned when a usage report
-- can\'t be generated.
lastReportGenerationExecutionError_errorMessage :: Lens.Lens' LastReportGenerationExecutionError (Prelude.Maybe Prelude.Text)
lastReportGenerationExecutionError_errorMessage = Lens.lens (\LastReportGenerationExecutionError' {errorMessage} -> errorMessage) (\s@LastReportGenerationExecutionError' {} a -> s {errorMessage = a} :: LastReportGenerationExecutionError)

instance
  Data.FromJSON
    LastReportGenerationExecutionError
  where
  parseJSON =
    Data.withObject
      "LastReportGenerationExecutionError"
      ( \x ->
          LastReportGenerationExecutionError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance
  Prelude.Hashable
    LastReportGenerationExecutionError
  where
  hashWithSalt
    _salt
    LastReportGenerationExecutionError' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage

instance
  Prelude.NFData
    LastReportGenerationExecutionError
  where
  rnf LastReportGenerationExecutionError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
