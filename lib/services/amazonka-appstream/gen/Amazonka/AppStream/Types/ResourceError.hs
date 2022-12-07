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
-- Module      : Amazonka.AppStream.Types.ResourceError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ResourceError where

import Amazonka.AppStream.Types.FleetErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource error.
--
-- /See:/ 'newResourceError' smart constructor.
data ResourceError = ResourceError'
  { -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The time the error occurred.
    errorTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The error code.
    errorCode :: Prelude.Maybe FleetErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'resourceError_errorMessage' - The error message.
--
-- 'errorTimestamp', 'resourceError_errorTimestamp' - The time the error occurred.
--
-- 'errorCode', 'resourceError_errorCode' - The error code.
newResourceError ::
  ResourceError
newResourceError =
  ResourceError'
    { errorMessage = Prelude.Nothing,
      errorTimestamp = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
resourceError_errorMessage :: Lens.Lens' ResourceError (Prelude.Maybe Prelude.Text)
resourceError_errorMessage = Lens.lens (\ResourceError' {errorMessage} -> errorMessage) (\s@ResourceError' {} a -> s {errorMessage = a} :: ResourceError)

-- | The time the error occurred.
resourceError_errorTimestamp :: Lens.Lens' ResourceError (Prelude.Maybe Prelude.UTCTime)
resourceError_errorTimestamp = Lens.lens (\ResourceError' {errorTimestamp} -> errorTimestamp) (\s@ResourceError' {} a -> s {errorTimestamp = a} :: ResourceError) Prelude.. Lens.mapping Data._Time

-- | The error code.
resourceError_errorCode :: Lens.Lens' ResourceError (Prelude.Maybe FleetErrorCode)
resourceError_errorCode = Lens.lens (\ResourceError' {errorCode} -> errorCode) (\s@ResourceError' {} a -> s {errorCode = a} :: ResourceError)

instance Data.FromJSON ResourceError where
  parseJSON =
    Data.withObject
      "ResourceError"
      ( \x ->
          ResourceError'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ErrorTimestamp")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable ResourceError where
  hashWithSalt _salt ResourceError' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorTimestamp
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData ResourceError where
  rnf ResourceError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorTimestamp
      `Prelude.seq` Prelude.rnf errorCode
