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
-- Module      : Network.AWS.IoT.Types.ValidationError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ValidationError where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an error found in a behavior specification.
--
-- /See:/ 'newValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The description of an error found in the behaviors.
    errorMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'validationError_errorMessage' - The description of an error found in the behaviors.
newValidationError ::
  ValidationError
newValidationError =
  ValidationError' {errorMessage = Core.Nothing}

-- | The description of an error found in the behaviors.
validationError_errorMessage :: Lens.Lens' ValidationError (Core.Maybe Core.Text)
validationError_errorMessage = Lens.lens (\ValidationError' {errorMessage} -> errorMessage) (\s@ValidationError' {} a -> s {errorMessage = a} :: ValidationError)

instance Core.FromJSON ValidationError where
  parseJSON =
    Core.withObject
      "ValidationError"
      ( \x ->
          ValidationError'
            Core.<$> (x Core..:? "errorMessage")
      )

instance Core.Hashable ValidationError

instance Core.NFData ValidationError
