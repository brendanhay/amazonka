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
-- Module      : Network.AWS.Lambda.Types.EnvironmentError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EnvironmentError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Error messages for environment variables that couldn\'t be applied.
--
-- /See:/ 'newEnvironmentError' smart constructor.
data EnvironmentError = EnvironmentError'
  { -- | The error message.
    message :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'environmentError_message' - The error message.
--
-- 'errorCode', 'environmentError_errorCode' - The error code.
newEnvironmentError ::
  EnvironmentError
newEnvironmentError =
  EnvironmentError'
    { message = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
environmentError_message :: Lens.Lens' EnvironmentError (Prelude.Maybe Prelude.Text)
environmentError_message = Lens.lens (\EnvironmentError' {message} -> message) (\s@EnvironmentError' {} a -> s {message = a} :: EnvironmentError) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code.
environmentError_errorCode :: Lens.Lens' EnvironmentError (Prelude.Maybe Prelude.Text)
environmentError_errorCode = Lens.lens (\EnvironmentError' {errorCode} -> errorCode) (\s@EnvironmentError' {} a -> s {errorCode = a} :: EnvironmentError)

instance Prelude.FromJSON EnvironmentError where
  parseJSON =
    Prelude.withObject
      "EnvironmentError"
      ( \x ->
          EnvironmentError'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable EnvironmentError

instance Prelude.NFData EnvironmentError
