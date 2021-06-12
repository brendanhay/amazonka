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
-- Module      : Network.AWS.AppStream.Types.ResourceError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ResourceError where

import Network.AWS.AppStream.Types.FleetErrorCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a resource error.
--
-- /See:/ 'newResourceError' smart constructor.
data ResourceError = ResourceError'
  { -- | The time the error occurred.
    errorTimestamp :: Core.Maybe Core.POSIX,
    -- | The error message.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code.
    errorCode :: Core.Maybe FleetErrorCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorTimestamp', 'resourceError_errorTimestamp' - The time the error occurred.
--
-- 'errorMessage', 'resourceError_errorMessage' - The error message.
--
-- 'errorCode', 'resourceError_errorCode' - The error code.
newResourceError ::
  ResourceError
newResourceError =
  ResourceError'
    { errorTimestamp = Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The time the error occurred.
resourceError_errorTimestamp :: Lens.Lens' ResourceError (Core.Maybe Core.UTCTime)
resourceError_errorTimestamp = Lens.lens (\ResourceError' {errorTimestamp} -> errorTimestamp) (\s@ResourceError' {} a -> s {errorTimestamp = a} :: ResourceError) Core.. Lens.mapping Core._Time

-- | The error message.
resourceError_errorMessage :: Lens.Lens' ResourceError (Core.Maybe Core.Text)
resourceError_errorMessage = Lens.lens (\ResourceError' {errorMessage} -> errorMessage) (\s@ResourceError' {} a -> s {errorMessage = a} :: ResourceError)

-- | The error code.
resourceError_errorCode :: Lens.Lens' ResourceError (Core.Maybe FleetErrorCode)
resourceError_errorCode = Lens.lens (\ResourceError' {errorCode} -> errorCode) (\s@ResourceError' {} a -> s {errorCode = a} :: ResourceError)

instance Core.FromJSON ResourceError where
  parseJSON =
    Core.withObject
      "ResourceError"
      ( \x ->
          ResourceError'
            Core.<$> (x Core..:? "ErrorTimestamp")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable ResourceError

instance Core.NFData ResourceError
