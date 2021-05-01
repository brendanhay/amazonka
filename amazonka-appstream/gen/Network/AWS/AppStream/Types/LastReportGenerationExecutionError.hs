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
-- Module      : Network.AWS.AppStream.Types.LastReportGenerationExecutionError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.LastReportGenerationExecutionError where

import Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the error that is returned when a usage report can\'t be
-- generated.
--
-- /See:/ 'newLastReportGenerationExecutionError' smart constructor.
data LastReportGenerationExecutionError = LastReportGenerationExecutionError'
  { -- | The error message for the error that is returned when a usage report
    -- can\'t be generated.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code for the error that is returned when a usage report can\'t
    -- be generated.
    errorCode :: Prelude.Maybe UsageReportExecutionErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LastReportGenerationExecutionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'lastReportGenerationExecutionError_errorMessage' - The error message for the error that is returned when a usage report
-- can\'t be generated.
--
-- 'errorCode', 'lastReportGenerationExecutionError_errorCode' - The error code for the error that is returned when a usage report can\'t
-- be generated.
newLastReportGenerationExecutionError ::
  LastReportGenerationExecutionError
newLastReportGenerationExecutionError =
  LastReportGenerationExecutionError'
    { errorMessage =
        Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message for the error that is returned when a usage report
-- can\'t be generated.
lastReportGenerationExecutionError_errorMessage :: Lens.Lens' LastReportGenerationExecutionError (Prelude.Maybe Prelude.Text)
lastReportGenerationExecutionError_errorMessage = Lens.lens (\LastReportGenerationExecutionError' {errorMessage} -> errorMessage) (\s@LastReportGenerationExecutionError' {} a -> s {errorMessage = a} :: LastReportGenerationExecutionError)

-- | The error code for the error that is returned when a usage report can\'t
-- be generated.
lastReportGenerationExecutionError_errorCode :: Lens.Lens' LastReportGenerationExecutionError (Prelude.Maybe UsageReportExecutionErrorCode)
lastReportGenerationExecutionError_errorCode = Lens.lens (\LastReportGenerationExecutionError' {errorCode} -> errorCode) (\s@LastReportGenerationExecutionError' {} a -> s {errorCode = a} :: LastReportGenerationExecutionError)

instance
  Prelude.FromJSON
    LastReportGenerationExecutionError
  where
  parseJSON =
    Prelude.withObject
      "LastReportGenerationExecutionError"
      ( \x ->
          LastReportGenerationExecutionError'
            Prelude.<$> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    LastReportGenerationExecutionError

instance
  Prelude.NFData
    LastReportGenerationExecutionError
