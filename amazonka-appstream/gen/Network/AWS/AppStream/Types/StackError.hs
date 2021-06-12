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
-- Module      : Network.AWS.AppStream.Types.StackError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackError where

import Network.AWS.AppStream.Types.StackErrorCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a stack error.
--
-- /See:/ 'newStackError' smart constructor.
data StackError = StackError'
  { -- | The error message.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code.
    errorCode :: Core.Maybe StackErrorCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'stackError_errorMessage' - The error message.
--
-- 'errorCode', 'stackError_errorCode' - The error code.
newStackError ::
  StackError
newStackError =
  StackError'
    { errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The error message.
stackError_errorMessage :: Lens.Lens' StackError (Core.Maybe Core.Text)
stackError_errorMessage = Lens.lens (\StackError' {errorMessage} -> errorMessage) (\s@StackError' {} a -> s {errorMessage = a} :: StackError)

-- | The error code.
stackError_errorCode :: Lens.Lens' StackError (Core.Maybe StackErrorCode)
stackError_errorCode = Lens.lens (\StackError' {errorCode} -> errorCode) (\s@StackError' {} a -> s {errorCode = a} :: StackError)

instance Core.FromJSON StackError where
  parseJSON =
    Core.withObject
      "StackError"
      ( \x ->
          StackError'
            Core.<$> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable StackError

instance Core.NFData StackError
