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
-- Module      : Network.AWS.IoT.Types.ValidationError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ValidationError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an error found in a behavior specification.
--
-- /See:/ 'newValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The description of an error found in the behaviors.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ValidationError' {errorMessage = Prelude.Nothing}

-- | The description of an error found in the behaviors.
validationError_errorMessage :: Lens.Lens' ValidationError (Prelude.Maybe Prelude.Text)
validationError_errorMessage = Lens.lens (\ValidationError' {errorMessage} -> errorMessage) (\s@ValidationError' {} a -> s {errorMessage = a} :: ValidationError)

instance Prelude.FromJSON ValidationError where
  parseJSON =
    Prelude.withObject
      "ValidationError"
      ( \x ->
          ValidationError'
            Prelude.<$> (x Prelude..:? "errorMessage")
      )

instance Prelude.Hashable ValidationError

instance Prelude.NFData ValidationError
